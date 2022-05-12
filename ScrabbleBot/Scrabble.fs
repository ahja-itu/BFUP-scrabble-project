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
        lastFailedPlay      : ((int * int) * (uint32 * (char * int))) list option
    }
    
    let mkState b d pn h cp sb = {board = b; dict = d;  playerNumber = pn; hand = h; consecutivePasses = cp; statefulBoard = sb; lastFailedPlay = None} 

    let board st            = st.board
    let dict st             = st.dict
    let playerNumber st     = st.playerNumber
    let hand st             = st.hand
    let consecutivePasses st= st.consecutivePasses
    let statefulBoard st    = st.statefulBoard
  

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =
        // Pieces is a map from a letter (represented in its index in the alphabet (1-indexed)) to its (char * int) pair

        
        let printStatefulBoard () =
            debugPrint "#################\nPrinting statefulboard content:\n"
//            for (c, lst) in StatefulBoard.getPlacedTilesAndPositons st.statefulBoard do
//                debugPrint (sprintf "Character %c is located here:\n" c)
//                for (x, y) in lst do
//                    debugPrint (sprintf "(%i, %i)\n" x y)
            debugPrint "##################\n\n\n"
        

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            // forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            
            printStatefulBoard ()
            
            // Determine whether or not we're at turn 1
            let isFirstRound =
                match getSquare st.board.center st.statefulBoard with
                | Some sq -> false
                | _ -> true
            
            let lettersInHand = Utils.handToLetters st.hand
           
            let playableLetters =
                if not isFirstRound then 
                    StatefulBoard.getPlacedTilesAndPositons st.statefulBoard
                else
                    lettersInHand |> Array.toList |> List.map (fun e -> (e, [st.board.center]))
                    
                    
            debugPrint (sprintf "Determining playable letters: %A\n" playableLetters)
            
            let mockHand =
                if isFirstRound then st.hand
                else playableLetters
                     |> List.map (fun (c, _) -> Utils.letterToNumber c)
                     |> List.fold (fun hand c -> MultiSet.addSingle c hand) (State.hand st)
            
            let mockHands =
                 playableLetters
                 |> List.map (fun (c, _) -> Utils.letterToNumber c)
                 |> List.map (fun c -> MultiSet.addSingle c st.hand)
                 |> Array.ofList
                 
            let playableLetters' =
                List.map fst playableLetters |> Array.ofList
                
            let someLetter = MultiSet.toList st.hand |> List.head
            let hand' = MultiSet.removeSingle someLetter st.hand
            let centerX, centerY = st.board.center

            // This is a function to attempt to find a playable word from one coordinate at the time
            let rec fastWordPlay () =
                match PointQuery.get () with
                | Some coord ->
                    
                    
                | None -> fastWordPlay ()
                
                
            
            
            
            
            let longestPossibleWord' =
                if isFirstRound then
                    WordSearch.findCandidateWords hand' (State.dict st) (Utils.numberToLetter someLetter)
                    |> List.fold (fun acc s -> if String.length acc < String.length s then s else acc) ""
                    |> Seq.mapi (fun i e -> ((centerX + i, centerY), ((Utils.letterToNumber e), Utils.pairLetterWithPoint e)))
                    |> Seq.toList
                else Array.Parallel.map (WordSearch.findCandidateWords st.hand (State.dict st) >> List.map (playFromWord st.statefulBoard st.board.squares st.hand)) playableLetters'
                     |> Array.toList
                     |> List.fold (@) []
                     |> List.filter (fun l -> List.length l > 0)
                     |> List.fold (fun acc x -> if List.length acc > List.length x then acc else x) []
            
            // Add all of the coordinates of the longest possible word to the point query queue
            // TODO: Could be async. But would that even save time for such small words?
            for (coord, _) in longestPossibleWord' do
                PointQuery.put(coord)
            
            debugPrint (sprintf "Will be attempting to play word: %A\n" longestPossibleWord')
            
            send cstream (SMPlay longestPossibleWord')
                
            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                debugPrint "CMPlaySuccess!!!\n"
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)

                // Define the function used in the foolowing fold to remove the played letters from the hand
                let removeSingleLetter hand' (_, (_, (letter, _))) =
                    MultiSet.removeSingle (Utils.letterToNumber letter) hand'

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
            | RCM (CMPlayFailed (pid, ms)) ->
                debugPrint "CMPlayFailed\n"
                (* Failed play. Update your state *)
                
                
                
                // Lets attempt not to repeat failed plays by saving the last failed play
                let st' = {st with lastFailedPlay = Some ms} 
                aux st'
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
                
                printStatefulBoard ()
                
                
                aux st

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
        