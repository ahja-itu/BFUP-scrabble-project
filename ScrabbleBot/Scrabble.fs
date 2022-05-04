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
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        statefulBoard : StatefulBoard
    }

    let mkState b d pn h sb = {board = b; dict = d;  playerNumber = pn; hand = h; statefulBoard = sb }

    let board st            = st.board
    let dict st             = st.dict
    let playerNumber st     = st.playerNumber
    let hand st             = st.hand
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
                        WordSearch.findCandidateWords letter (State.hand st) (State.dict st)
                        |> List.fold Utils.longestStringOf "")
                |> Array.fold Utils.longestStringOf ""

            debugPrint (sprintf "Longest word to play: %s\n" wordToPlay)
            let input =  System.Console.ReadLine()

            // TODO: Find candidate words to play
            
        

            let move = RegEx.parseMove input

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

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

                let hand' = List.fold removeSingleLetter (State.hand st) move
                debugPrint (sprintf "Received new pieces: %A\n" newPieces)

                // Recive letters and update hand
                
                let st' = {st with hand = hand'}   // This state needs to be updated, (hand, board, turn)

                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be updated, only board (and possibly player number/turn)
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated  
                aux st'
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

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet (mkStatefulBoard()))
        