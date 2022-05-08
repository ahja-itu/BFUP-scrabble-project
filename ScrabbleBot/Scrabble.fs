namespace EmmaGaddagBot

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
    }

    let mkState b d pn h cp sb = {board = b; dict = d;  playerNumber = pn; hand = h; consecutivePasses = cp; statefulBoard = sb } 

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


        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            let wordToPlay = 
                Utils.handToLetters (State.hand st) |>
                Array.Parallel.map 
                    (fun letter -> 
                        WordSearch.findCandidateWords (State.hand st) (State.dict st) letter
                        |> List.fold Utils.longestStringOf "")
                |> Array.fold Utils.longestStringOf ""

            debugPrint (sprintf "Longest word to play: %s\n" wordToPlay)
            

            //Generate input
            let lettersOnBoard = StatefulBoard.getPlacedTilesAndPositons 
            let letter = 'e' // TODO: Change this to actual statefulTile from StatefulBoard
            let letterPosition = (3,4)
            let letterOrientation = Horizontal
            let candidateWords = letter |> WordSearch.findCandidateWords (State.hand st) (State.dict st) 

            let candidateWordsAsTiles = List.map (fun candidateWord -> candidateWord |> Seq.map (fun c -> (c, snd (Utils.pairLetterWithPoint (string c)))) |> Seq.toList) candidateWords


            let wordToPlayResult = playWord letterPosition letterOrientation candidateWordsAsTiles (State.statefulBoard st)
            let (result, word, position) = wordToPlayResult
            let wordToPlay3 = Some(word, position)

            let generateInput1 ((tile, coord) : (char*int)*(int * int)) : string =
                let builder = System.Text.StringBuilder()
                builder.Append("(")                                     |> ignore
                builder.Append(fst coord)                        |> ignore
                builder.Append(" ")                                      |> ignore
                builder.Append(snd coord)                        |> ignore
                builder.Append(" ")                                      |> ignore
                builder.Append(Utils.letterToNumber (fst tile))  |> ignore
                builder.Append(fst tile)                         |> ignore
                builder.Append(snd tile )                        |> ignore
                builder.Append(")")                                      |> ignore
            
                builder.ToString()

            let generateInput (word: (char * int) list) (position: (int * int) list): string =
                let wordPosition = List.zip word position 
                let inputList = List.map (fun wp -> generateInput1 wp) wordPosition
                //let inputList = Seq.map (fun c -> (generateInput1 c (0,0))) word |> Seq.toList
                String.concat " " inputList

            let moveToPlay wtp: ServerMessage =
                match wtp with
                    | None _ -> SMPass // TODO check that sending this and receiving answer clientMessage is handled
                    | Some (wordPlayed, wordPosition) ->         
                        let input = generateInput wordPlayed wordPosition
                        let move = RegEx.parseMove input
                        debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) 
                        SMPlay move

            send cstream (moveToPlay wordToPlay3)

            //Move to utils or StatefulBoard? Needs the WordOrientation enum

            //let wordToPlay2 = playWord(WordSearch.StatefulBoard.getPlacedTilesAndPositons (State.hand st) (State.dict st))

            //let input =  System.Console.ReadLine()

            // TODO: Find candidate words to play
            
            //let move = RegEx.parseMove input

            //debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            //send cstream (SMPlay move) 

            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)

                // Update the board from the played words
                // Fra move kender vi hvad der er blevet spillet. 
                // let boardTemp = st'.board
                // let updateBoard (b: State.board) = 
                
            
                // Update the hand from the played words
                // Remove succefully played letter

                let charToAlphaIndex (c: char) : uint32 =
                    (uint32 c) - 64u

                let removeSingleLetter hand' (_, (_, (letter, _))) =
                    MultiSet.removeSingle (charToAlphaIndex letter) hand'

                let hand' = List.fold removeSingleLetter (State.hand st) 
                debugPrint (sprintf "Received new pieces: %A\n" newPieces)

                // Recive letters and update hand
                
                let st' = {st with hand = hand'}   // This state needs to be updated, (hand, board, turn)

                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let (coords, tiles) = List.unzip ms
                let (tileIDs, word) = List.unzip tiles
                let insertResult = StatefulBoard.insertWord coords[0] (getOrientation coords) word st.statefulBoard  
                

                let st' = st // This state needs to be updated, only board (and possibly player number/turn)
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated  
                aux st'
            | RCM (CMPassed _) -> // TODO keep track of consecutive passes, if 3 end game
                let st' = {st with consecutivePasses = st.consecutivePasses+1u}
                if st'.consecutivePasses>=3u then () //Game over
                                             else aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st

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
        