namespace EmmaGaddagBot

module StatefulBoard =

    open System.Collections.Generic
    open ScrabbleUtil
    open ScrabbleUtil.ServerCommunication

    type WordOrientation = Horizontal | Vertical
    type StatefulSquare = {
        word: (char * int) list         // All characters in the word, and the associated point value
        pos : int                       // The position in the word in which the letter is located
        letter: (char * int)            // Character and point
        orientation : WordOrientation   // The orientation of the word, in respect to the board
    }

    // first dictionary: coordinates to a square
    // second dictionary: letters to coordinates where they can be found
    type StatefulBoard = SB of Dictionary<(int * int), StatefulSquare> * Dictionary<char, (int * int) list>
    
    let mkStatefulBoard () =
        SB (Dictionary<(int * int), StatefulSquare>(), Dictionary<char, (int * int) list>())
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    // Query the board
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    let getSquare (coords: (int * int)) (SB (board, letters): StatefulBoard) : StatefulSquare option = 
        match board.TryGetValue(coords) with
        | true, statefulSquare -> Some statefulSquare
        | false, _ -> None


    let getPlacedTilesAndPositons (SB (_, letters): StatefulBoard) : (char * (int * int) list) list =
        [for key in letters.Keys -> (key, letters.[key])]

    let getPlacedTilesAndPositonsForChar (c: char) (SB (_, letters): StatefulBoard) : (int * int) list =
        match letters.TryGetValue(c) with
        | true, coords -> coords
        | false, _ -> []


    let isSome (x: 'a option) : bool =
        match x with
        | Some _ -> true
        | None -> false

    let unwrapSome (x: 'a option) : 'a =
        match x with
        | Some x -> x
        | None -> failwith "unwrapSome: None"

    let findIndices (word: (char * int) list) (c: char) : int list =
        List.mapi (fun i (e, _) -> if c = e then Some i else None) word
        |> List.filter (fun i -> isSome i)
        |> List.map unwrapSome


    let isSquareEmpty (coords: (int * int)) (statefulBoard: StatefulBoard) : bool =
        not <| isSome (getSquare coords statefulBoard)

    /// In this context, the posision is where the word starts
    let determineCoordinates (word: (char * int) list) ((x, y): (int * int)) (orientation: WordOrientation) : (int * int) list =
        let aux i _ =
            match orientation with
            | Horizontal -> (x + i, y)
            | Vertical -> (x, y + i)
        
        List.mapi aux word

    let determineSufficientSpace (word: (char * int) list) ((x, y) : (int * int)) (orientation: WordOrientation) (statefulBoard: StatefulBoard) : bool =
        let coords = determineCoordinates word (x, y) orientation
        // TODO: perhaps check if coordinates are within the board 
        let hasSpace = List.forall (fun (x, y) -> isSquareEmpty (x, y) statefulBoard) coords
        hasSpace

    let checkCollisions (word: (char * int) list) ((x, y): (int * int)) (SB (board, letters): StatefulBoard) : ((int * int) * WordOrientation) option =    
        match getSquare (x, y) (SB (board, letters)) with
        | Some statefulSquare ->
            let candidateIndices = fst statefulSquare.letter |> findIndices word
            
            None
        | None -> Some ((x, y), Horizontal)

    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    // Inserting stuff
    // @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

    // TODO: Check for collitions, because this does not do this before inserting
    let insertWord (coords: (int * int)) (wordOrientation: WordOrientation) (word: (char * int) list) (SB (board, letters): StatefulBoard) : Result<StatefulBoard, GameplayError> = 
        let rec aux (pos: int) =
            let coords' = 
                match wordOrientation with
                | Vertical -> (fst coords, snd coords + pos)
                | Horizontal -> (fst coords + pos, snd coords)

            match board.TryGetValue (coords') with
            | true, placedLetter -> 
                Error (GPEOccupiedTile ((word.[pos], coords', placedLetter.letter)))
            | false, _ -> 
                board.Add(coords', {letter = word.[pos]; word = word; pos = pos; orientation = wordOrientation})
                
                let coordList = 
                    match letters.TryGetValue(fst word.[pos]) with
                    | true, list -> coords' :: list
                    | false, _ -> [coords']

                letters.Add(fst word.[pos], coordList)

                if pos < word.Length 
                then aux (pos + 1) 
                else Ok (SB (board, letters))

        aux 0


