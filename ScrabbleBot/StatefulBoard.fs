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

    (*
    let isSquareEmpty (coords: (int * int)) (statefulBoard: StatefulBoard) : bool =
        not <| isSome (getSquare coords statefulBoard)*)

    //Takes coordinate and checks whether it is isnide the current board
    let isInsideBoard (coord: (int * int)) (SB (board, letters): StatefulBoard) : bool =
        board.ContainsKey(coord)


    // 5/5
    let isSquareEmptyOrMatch (c : (char * int)) (coords: (int * int)) (statefulBoard: StatefulBoard) : bool =
        match ((getSquare coords statefulBoard)) with  
            | None -> true
            | Some sSquare -> sSquare.letter=c
            
    
    // TODO: Add method that takes char and position or stateful square and calls e.g. determineCoordinates with right orientation                                   

                     
    /// In this context, the posision is where the word starts. Returns list of all coordinates, that would be occupied by the given word
    (*let determineCoordinates (word: (char * int) list) ((x, y): (int * int)) (orientation: WordOrientation) : (int * int) list =
        let aux i _ =
            match orientation with
            | Horizontal -> (x + i, y)
            | Vertical -> (x, y + i)
        
        List.mapi aux word*)

    // 5/5
    // Take word, letter to match, coordinate of letter to match, orientation and return list of possible coordinates to occupy
    let determineCoordinatesWithDuplicates (c : (char * int))(word: (char * int) list)  ((x, y): (int * int)) (orientation: WordOrientation) : (int * int) list list =
       
        let mapCoordinates i (i', _) =
            match orientation with
            | Horizontal -> ((x-i') + i, y)
            | Vertical -> (x, (y-i') + i)

        let mapLettersInWord i c' =
            match c=c' with
                | true -> List.map (fun c -> (i,c)) word |> List.mapi mapCoordinates
                | false -> List.empty
            
        List.mapi mapLettersInWord word
    


    // Take word, first-letter coordinates, orientation and board into account and check wheter word could be placed here
    (*let determineSufficientSpace (word: (char * int) list) ((x, y) : (int * int)) (orientation: WordOrientation) (statefulBoard: StatefulBoard) : bool =
        let coords = determineCoordinates word (x, y) orientation
        // TODO: perhaps check if coordinates are within the board 
        let hasSpace = List.forall (fun (x, y) -> isSquareEmpty (x, y) statefulBoard) coords
        hasSpace*)

    // 5/5
    let determineSufficientSpaceOrMatch (word: (char * int) list) ((x: int, y) : (int * int)) (orientation: WordOrientation) (statefulBoard: StatefulBoard) (coords: (int * int) list) : bool =
        //let coords = determineCoordinates word (x, y) orientation //TODO change to use determineCoordinatesWithDuplicates instead
        
        // TODO: perhaps check if coordinates are within the board 
        let hasSpaceOrMatchHorizontal = List.forall (fun (x', y') -> isSquareEmptyOrMatch word[x'-x] (x', y') statefulBoard) coords
        let hasSpaceOrMatchVertical = List.forall (fun (x', y') -> isSquareEmptyOrMatch word[y'-y] (x', y') statefulBoard) coords

        match orientation with 
            | Horizontal -> hasSpaceOrMatchHorizontal
            | Vertical -> hasSpaceOrMatchVertical
    
    //Takes the char from the board, the word that should match, the coordinates for the char, orientation, statefulBoard.
    //Returns the coordinates for a valid word with a valid position
    let validWord (c: (char * int)) (word: (char * int) list) ((x,y): (int * int)) (orientation: WordOrientation) (statefulBoard: StatefulBoard): ((char*int) list * (int*int) list) option=
        let listNotEmpty (l: 'a list) : bool = if l = [] then false else true
        let coordsLists : ((int * int) list) list = 
            determineCoordinatesWithDuplicates c word (x,y) orientation
        
        let notEmptyCoordsLists = List.filter listNotEmpty coordsLists

        let collisionPred =
            fun l -> (determineSufficientSpaceOrMatch word (x,y) orientation statefulBoard l)
        
        let coordsList = notEmptyCoordsLists |> List.tryFind (fun elm -> collisionPred elm = true)
        match coordsList with
            | Some(cl) -> Some(word, cl)
            | _ -> None
        
        

    // Take word, first letter position,     
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

    /////////////////////////////////////////////////////////////////////////////
    
        
    // TODO adjust input to this function to fit what MyFunction actually needs
    let playWord (c : char * int)(coords: (int * int)) (wordOrientation: WordOrientation) (words: (char * int) list list) (SB (board, letters): StatefulBoard) : (Result<StatefulBoard, GameplayError> * (char *int) list * (int * int) list) =
        let foundWords = List.map (fun word -> (validWord c word coords wordOrientation (SB (board, letters)))) words//breaks when MyFunction returns true aka a word has been inserted
        let firstFoundWord = List.tryFind (fun foundWord -> isSome(foundWord)) foundWords //Why is foundWord here not wrapped in option? foundWords is list of options
        let insertResult = insertWord (snd unwrapSome(unwrapSome(firstFoundWord))[0]) wordOrientation firstFoundWord SB(board, letters)
        (insertResult, firstFoundWord)


    let playValidWord (c: (char * int)) (words: (char * int) list list) ((x,y): (int * int)) (orientation: WordOrientation) (statefulBoard: StatefulBoard): (int*int) list option=
        List.tryFind (fun word -> 
        
        
        
        let listNotEmpty (l: 'a list) : bool = if l = [] then false else true
        let coordsLists = 
            List.map (fun word -> determineCoordinatesWithDuplicates c word (x,y) orientation) words
        
        let notEmptyCoordsLists = List.filter listNotEmpty coordsLists

        let collisionPred =
            fun l -> (determineSufficientSpaceOrMatch word (x,y) orientation statefulBoard l)
        
        notEmptyCoordsLists |> List.tryFind (fun elm -> collisionPred elm = true) 

