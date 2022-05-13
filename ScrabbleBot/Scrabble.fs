namespace EmmaGaddagBot

open System.Collections.Immutable
open System.Drawing
open Parser
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

open StatefulBoard

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        // x, i: letter, how many of them do we have
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

 

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.
    type state = {
        board               : Parser.board
        dict                : ScrabbleUtil.Dictionary.Dict
        playerNumber        : uint32
        hand                : MultiSet.MultiSet<uint32>
        consecutivePasses   : uint32
        statefulBoard       : StatefulBoard
        piecesToSwap        : int
    }
    
    let mkState b d pn h cp sb = {board = b; dict = d;  playerNumber = pn; hand = h; consecutivePasses = cp; statefulBoard = sb; piecesToSwap = (int << MultiSet.size) h } 

    let board st            = st.board
    let dict st             = st.dict
    let playerNumber st     = st.playerNumber
    let hand st             = st.hand
    let consecutivePasses st= st.consecutivePasses
    let statefulBoard st    = st.statefulBoard
  

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =
        // PREGAME SETUP
        // Put the center coordinate into the PointQuery queue before starting
        PointQuery.put(st.board.center)
        
        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            
            // Determine whether or not we're at turn 1
            let isFirstRound =
                match getSquare st.board.center st.statefulBoard with
                | Some sq -> false
                | _ -> true
                            
            // We chose the last letter in the hand to avoid wild cards, since they cause problems on the first move
            let someLetter = MultiSet.toList st.hand |> List.last
            let hand' = st.hand
            let centerX, centerY = st.board.center
            
            let firstRoundPlay () =
                WordSearch.findCandidateWords hand' st.dict (Utils.numberToLetter someLetter)
                |> List.fold (Utils.chooseHighestScoringWord) []
                |> Seq.mapi (fun i e ->
                    debugPrint (sprintf "Found character to play was: %A\n" e)
                    match e with
                    | c, 0 -> ((centerX + i, centerY), (0u, (c, 0)))
                    | c, _ -> ((centerX + i, centerY), (Utils.letterToNumber c, Utils.pairLetterWithPoint c)))
                |> Seq.toList
            
            
            PointQuery.print ()
            
            // This is a function to attempt to find a playable word from one coordinate at the time
            let rec singleWordPlay () =
                match PointQuery.get () with
                | Some coord ->
                    match getSquare coord st.statefulBoard with
                        | Some sq ->
                             let rec chooseFirstAvailablePlay candidateWords =
                                 match candidateWords with
                                 | [] -> ([], [])
                                 | word :: words -> match playFromWord st.statefulBoard st.board.squares st.hand (fst sq.letter, coord) word with
                                                    | ([], []) -> chooseFirstAvailablePlay words
                                                    | play -> play
                             let candidate =
                                 sq.letter
                                 |> fst
                                 |> Utils.letterToNumber
                                 |> fun l -> MultiSet.addSingle l st.hand
                                 |> fun mockHand -> WordSearch.findCandidateWords mockHand st.dict (fst sq.letter)
                                 |> List.rev
                                 |> chooseFirstAvailablePlay
                                 
                             match candidate with
                             | (_, []) -> singleWordPlay ()
                             | res -> res
                        | None ->
                            if isFirstRound then ([], firstRoundPlay ()) else singleWordPlay ()
                | None -> ([], [])
            
            
            // Look at this fancy parallelism
            let (coordinatesToRemove, theWordCandidate) = 
                [0..19]
                |> List.map (fun _ -> async { return singleWordPlay () })
                |> Async.Parallel
                |> Async.RunSynchronously
                |> Array.toList
                |> List.fold (fun (maxCoords, max) (currCoords, curr) -> if List.length max < List.length curr then (currCoords, curr) else (maxCoords, max)) ([], [])
                    
            let longestPossibleWord' = theWordCandidate
            PointQuery.resetAfterRound ()
            
            
            for coord in coordinatesToRemove do
                PointQuery.put(coord)
                
            for (coord, _) in longestPossibleWord' do
                PointQuery.put(coord)
            
            debugPrint (sprintf "Will be attempting to play word: %A\n" longestPossibleWord')
            
            
            match longestPossibleWord' with
                 | [] ->
                     if st.piecesToSwap = 0
                        then send cstream SMForfeit
                        else send cstream (SMChange (MultiSet.toList st.hand |> List.take st.piecesToSwap)) // yikes
                 | _ -> send cstream (SMPlay longestPossibleWord') 
                
            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                debugPrint "CMPlaySuccess!!!\n"
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)

                // Define the function used in the foolowing fold to remove the played letters from the hand
                let removeSingleLetter hand' (_, (_, (letter, point))) =
                    match point with
                    | 0 -> MultiSet.removeSingle 0u hand'
                    | _ -> MultiSet.removeSingle (Utils.letterToNumber letter) hand'

                // Actually remove the letters from the hand, produce a new version of the hand
                let hand' = 
                    List.fold removeSingleLetter (State.hand st) ms

                // Add the letters that were given from the server
                debugPrint "Updating hand\n"
                let hand'' = List.fold (fun handy (letter, count) -> MultiSet.add letter count handy) hand' newPieces
                
                // The play from earlier was successful, so we can update the board
                debugPrint "Updating stateful board"
                
                let (coords, charinfo) = List.unzip ms
                let (_, word) = List.unzip charinfo
                let orientation = determineDirectionOfPlayedWord ms st.statefulBoard
                
                
                let sb' = StatefulBoard.insertWord word coords orientation st.statefulBoard
                
                debugPrint "Creating new state"
                // Update the state with the finalized hand
                let st' = {st with hand = hand''; statefulBoard = sb'}

                debugPrint "Returning from CMPlaySuccess"
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                debugPrint "CMPlayed!! Updating\n"
                (* Successful play by other player. Update your state *)
                
                if pid = st.playerNumber then
                    aux st
                else
                    let (coords, charinfo) = List.unzip ms
                    let (_, word) = List.unzip charinfo
                    let orientation = determineDirectionOfPlayedWord ms st.statefulBoard
                    let sb' = StatefulBoard.insertWord word coords orientation st.statefulBoard
                    let st' = {st with statefulBoard = sb'}
                    aux st'
             | RCM (CMChangeSuccess (newTiles)) -> 
                debugPrint "CMChangeSucces\n"
                // let hand' = MultiSet.toList st.hand |> List.head |> fun letter -> MultiSet.removeSingle letter st.hand
                let hand'' = List.fold (fun handy (letter, count) -> MultiSet.add letter count handy) (MultiSet.ofList []) newTiles //from CMPlaySuccess case
                let st' = {st with hand = hand''}

                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                debugPrint "CMPlayFailed\n"
                aux st
            | RCM (CMPassed _) -> // TODO keep track of consecutive passes, if 3 end game
                debugPrint "CMPassed\n"

                let st' = {st with consecutivePasses = st.consecutivePasses+1u}
                if st'.consecutivePasses>=3u then () //Game over
                                             else aux st'
            | RCM (CMGameOver _) ->
                debugPrint "CMGameOver\n"
                ()
            | RCM a ->
                debugPrint (sprintf "%A\n" a)
                failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                debugPrint (sprintf "RGPE err: %A\n" err)
                printfn "Gameplay Error:\n%A" err
                
                if List.contains GPEEmptyMove err
                then PointQuery.print () else ()
                
                let reactToError st' err' =
                    match err' with
                    | GPENotEnoughPieces (_, actuallyLeft) -> {st with piecesToSwap = int actuallyLeft}
                    | _ -> st'
                
                let st' = List.fold reactToError st err
                        
                
                
                aux st'

        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)
        let dict = dictf true // Uncomment if using a gaddag for your dictionary
        // let dict = dictf false // Uncomment if using a trie for your dictionary
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let board = Parser.mkBoard boardP
        stopWatch.Stop()
        printfn "Stopwatch time: %f\n" stopWatch.Elapsed.TotalMilliseconds
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet 0u (mkStatefulBoard()))
        