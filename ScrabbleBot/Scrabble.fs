namespace EmmaGaddagBot

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
            for (c, lst) in StatefulBoard.getPlacedTilesAndPositons st.statefulBoard do
                debugPrint (sprintf "Character %c is located here:\n" c)
                for (x, y) in lst do
                    debugPrint (sprintf "(%i, %i)\n" x y)
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
            
            let playFromWord (candidateWord: string) : ((int * int) * (uint32 * (char * int))) list = 
                // debugPrint (sprintf "candidate word to play: %A\n" candidateWord)
                let word = candidateWord.ToCharArray() |> Array.toList |> List.map (Utils.pairLetterWithPoint) |> Seq.toList
                // debugPrint (sprintf "Constructed word to play: %A\n" word)
                
                // Filter playable letters by the letters of the word
                
                let playableLetters' = List.filter (fun (c, _) -> String.exists (fun c' -> c = c') candidateWord) playableLetters                

                let isNotBothWordDirections coords =
                    match getSquare coords st.statefulBoard with
                    | Some sq -> not (sq.orientation = Both)
                    | _ -> true
                    
                let getOppositeDirection coords =
                    match getSquare coords st.statefulBoard with
                    | Some sq -> StatefulBoard.oppositeOrientation sq.orientation
                    | _ -> Horizontal
                
                let rec findFirstPossibleWordPlay playableLetters' =
                    match playableLetters' with
                    | [] -> None
                    | (c, lst) :: cs ->
                        // debugPrint (sprintf "Considering plays for %c\n" c)
                        let r = lst |> List.filter (isNotBothWordDirections)
                                    |> List.map (fun e ->
                                         // debugPrint (sprintf "Mapping coordinates: %A -> %A\n" c e)
                                         StatefulBoard.possibleWordPlacements e word (getOppositeDirection e) st.statefulBoard st.board.squares)
                        
                        let candidate = List.tryFind (fun e ->
                            match e with
                            | Some lst -> true
                            | _ -> false) r
                        
                        // debugPrint (sprintf "Try finding candidate: %A\n" candidate)
                        
                        match candidate with
                        | Some s -> match s with
                                    | Some q ->
                                        match q with
                                        | [] -> findFirstPossibleWordPlay cs
                                        | _ -> Some (List.head q) 
                                        
                                    | None -> findFirstPossibleWordPlay cs
                        | None -> findFirstPossibleWordPlay cs

                // debugPrint "Going to find first possible word play\n"
                let playPayload = findFirstPossibleWordPlay playableLetters
                match playPayload with
                | Some payload ->
                    // debugPrint (sprintf "playPayload: %A\n" playPayload)                
                    // debugPrint (sprintf "Constucting move on word: %A, coords: %A\n" payload.word payload.coordinates)  
                    //        this is the characters that gets connected to their id    this is connecting the coords before the characters
                    
                    if (List.length payload.word) = (List.length payload.coordinates) then
                        
                        let move = List.map (fun tuple -> (Utils.letterToNumber (fst tuple), tuple)) payload.word |> List.zip payload.coordinates
                        // debugPrint (sprintf "Returning candidate move: %A\n" move)
                        move
                    else
                        // debugPrint (sprintf "Detected discrepancy in list lengts for coords and word. Word: %A, coords: %A\n" payload.word payload.coordinates)
                        []
                | None ->
                    // debugPrint (sprintf "No payload available for word %A\n" word)
                    []
            
            
            let mockHand =
                if isFirstRound then st.hand
                else playableLetters
                     |> List.map (fun (c, _) -> Utils.letterToNumber c)
                     |> List.fold (fun hand c -> MultiSet.addSingle c hand) (State.hand st)
            
            let longestPossibleWord =
                Array.Parallel.map (WordSearch.findCandidateWords mockHand (State.dict st)) lettersInHand
                |> Array.Parallel.map (fun a -> List.map playFromWord a)
                |> Array.toList
                |> List.fold (@) []
                |> List.filter (fun l -> List.length l > 0 && (List.fold (fun (hand, b) (_, (_, (c, _))) ->
                    // This is fucking janky
                    // This is an additional check to see if the player has all the letters in the hand
                    if MultiSet.contains (Utils.letterToNumber c) hand then
                        (MultiSet.removeSingle (Utils.letterToNumber c) hand, b)
                    else (st.hand, false)) (st.hand, true) l) |> snd)
                |> List.filter (fun word ->
                        match st.lastFailedPlay with
                        | None -> true
                        | Some word' -> not (word = word'))
                |> List.fold (fun acc x -> if List.length acc > List.length x then acc else x) []
            
            debugPrint (sprintf "Will be attempting to play word: %A\n" longestPossibleWord)
            
            send cstream (SMPlay longestPossibleWord)
                
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
                let orientation = Utils.determineDirectionOfPlayedWord ms st.statefulBoard
                
                
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
                    let orientation = Utils.determineDirectionOfPlayedWord ms st.statefulBoard
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
        