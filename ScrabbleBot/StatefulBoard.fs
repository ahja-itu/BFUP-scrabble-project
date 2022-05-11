namespace EmmaGaddagBot


open System.Collections.Generic
open Parser
open ScrabbleUtil.Dictionary
module internal StatefulBoard =

    type WordOrientation = Horizontal | Vertical | Both
    type StatefulSquare = {
        word: (char * int) list         // All characters in the word, and the associated point value
        pos : int                       // The position in the word in which the letter is located
        letter: (char * int)            // Character and point
        orientation : WordOrientation   // The orientation of the word, in respect to the board
    }
    
    type WordInsertPayload = {
        word: (char * int) list
        coordinates: (int * int) list
        orientation: WordOrientation
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
        
    let positionIsNotWithBothWordDirections : StatefulBoard -> int * int -> bool
        = fun sb coords  ->
            let (SB (board, letters)) = sb
            match getSquare coords sb with
            | Some sq -> not (sq.orientation = Both)
            | None -> false
           
    let getPlacedTilesAndPositons (sb: StatefulBoard) : (char * (int * int) list) list =
        let (SB (board, letters)) = sb
        [for key in letters.Keys -> (key, letters.[key])]
        |> List.map(fun (c, lst) -> (c, List.filter (positionIsNotWithBothWordDirections sb) lst))

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
        
        
    let oppositeOrientation : WordOrientation -> WordOrientation
        = function
          | Vertical -> Horizontal
          | Horizontal -> Vertical
          | Both -> Both

    let findIndices (word: (char * int) list) (c: char) : int list =
        List.mapi (fun i (e, _) -> if c = e then Some i else None) word
        |> List.filter (fun i -> isSome i)
        |> List.map unwrapSome

    let isSquareAvailable (coords: int * int) (squares: boardFun2) : bool =
        match squares coords with
        | StateMonad.Success _-> true
        | q ->
            // DebugPrint.debugPrint (sprintf "Failed isSquareAviable with %A\n" q)
            false
            
    let hasSufficientSpaceAround : int * int -> StatefulBoard -> bool
        = fun (x, y) sb ->
            
            // If there is any tiles placed orthogonal to the word, then we don't want to place the word,
            // this is the "easy" solution instead of checking to see if they create valid words
            
            let surroundingSquares = [(x, y - 1); (x, y + 1); (x - 1, y); (x + 1, y)]
            //      x
            //   x  o  x
            //      x
                        
            // Go though the neighboring orthogonally placed squares
            // Should there be placed squares they will be of type Some square
            // we will then verify that there were none
            
            List.map (fun coord -> getSquare coord sb) surroundingSquares
            |> List.forall (not << isSome)
            
            
    let isSquarePlaceable (coords: (int * int)) (statefulBoard: StatefulBoard) (squares: boardFun2) : bool =
        // A square is "placeable" if it is a valid square in the board program, it doesn't have any tiles on it
        // in our own stateful board
        match ((getSquare coords statefulBoard)) with  
            | None -> true
            | _ -> false
        |> (&&) (isSquareAvailable coords squares)
        |> (&&) (hasSufficientSpaceAround coords statefulBoard)
    
    let isSingleSquarePlacable (coords: (int * int)) (statefulBoard: StatefulBoard) (squares: boardFun2) : bool =
        // A square is "placeable" if it is a valid square in the board program, it doesn't have any tiles on it
        // in our own stateful board
        match ((getSquare coords statefulBoard)) with  
            | None -> true
            | _ -> false
        |> (&&) (isSquareAvailable coords squares)
    
    let allowNeightboringPositions : int * int -> int * int -> StatefulBoard -> boardFun2 -> bool
        = fun (x, y) (x', y') sb squares ->
            let (a, b) = (x - x', y - y')
            let domain = [-1; 1]
            
            // TODO få squares adgang her til
            
            let coordsToCheck = [(x, y - 1); (x, y + 1); (x - 1, y); (x + 1, y)]
                                |> List.filter (fun c -> not (c = (x, y)))
            
            
            
            (List.contains a domain <> List.contains b domain) && List.forall (fun coord -> isSingleSquarePlacable coord sb squares) coordsToCheck
    
    
    let wordToString : (char * int) list -> string
        = fun word -> List.map fst word |> List.map string |> List.fold (+) ""
    
    let preventExtensionsOfPreplacedWord : int * int -> (int * int) list -> WordOrientation -> StatefulBoard -> bool 
        = fun (x, y) coords orientation sb ->
            
            // We have the coordinates to the "root" point where we are wanting to put the next word
            // Now we want to make sure that we're not extending the word into something that is not
            // a proper word in the games dictionary
    
            match getSquare (x, y) sb with
            | Some sq' ->
                
                true
                
            | None -> true 
            
    // Take word, letter to match, coordinate of letter to match, orientation and return list of possible coordinates to occupy
    let determineCoordinatesWithDuplicates (c : char) (word: (char * int) list) ((x, y): (int * int)) (orientation: WordOrientation) (sb: StatefulBoard) (squares: boardFun2): (int * int) list list =    
        let mapCoordinates orientation' i (i', _) =
            match orientation' with
            | Horizontal -> ((x-i') + i, y)
            | Vertical -> (x, (y-i') + i)
            | Both -> failwith "should not have happen"
        
        let mapLettersInWord i (c', _) =
            match c = c' with
                | true ->
                    match orientation with
                    | Both -> 
                        let buffer = List.map (fun c -> (i,c)) word
                        
                        let horizontalList = List.mapi (mapCoordinates Horizontal) buffer
                        let verticalList = List.mapi (mapCoordinates Vertical) buffer
                        horizontalList @ verticalList
                    | either -> List.map (fun c -> (i,c)) word |> List.mapi (mapCoordinates either)
                | false -> List.empty
            
        List.mapi mapLettersInWord word
        |> List.filter (List.length >> (<=) 1)
        |> List.filter (fun coords ->
            List.filter (fun coord -> (x, y) <> coord) coords // only consider coordinates that are not where the "root" of the word is
            |> List.forall (fun coordSet ->
                // also prevent extensions of worrds
                //preventExtensionsOfPreplacedWord (x, y) [] orientation sb &&
                (isSquarePlaceable coordSet sb squares || 
                allowNeightboringPositions (x, y) coordSet sb squares))) // If all remaining coordinates are free to have tiles placed there, then its good
    
    
//    let determineSufficientSpaceOrMatch (word: (char * int) list) ((x, y) : (int * int)) (orientation: WordOrientation) (statefulBoard: StatefulBoard) (squares : boardFun2) (coords: (int * int) list) : bool =
//        let hasSpaceOrMatchHorizontal = List.forall (fun (x', y') -> isSquareEmptyOrMatch word[x'-x] (x', y') orientation statefulBoard squares) coords
//        let hasSpaceOrMatchVertical = List.forall (fun (x', y') -> isSquareEmptyOrMatch word[y'-y] (x', y') orientation statefulBoard squares) coords
//
//        match orientation with 
//            | Horizontal -> hasSpaceOrMatchHorizontal
//            | Vertical -> hasSpaceOrMatchVertical
//            | Both -> false
    
    let possibleWordPlacements : int * int -> (char * int) list -> WordOrientation -> StatefulBoard -> boardFun2 -> WordInsertPayload list option
        = fun coord word orientation board squares ->
            
            let rec aux pos coords' acc posToRemove =
                match coords' with
                | c :: cs->
                    match getSquare c board with
                    | Some sq -> aux (pos + 1) cs acc (pos :: posToRemove) 
                    | None -> aux (pos + 1) cs (c :: acc) posToRemove
                | _ -> (List.rev acc, posToRemove)
            
            
            let removePosFromWord word posToRemove =
                let indicesToKeep = 
                    [0..List.length word]
                    |> List.filter (fun e -> List.contains e posToRemove)
                List.mapi (fun a b -> if List.contains a indicesToKeep then None else Some b) word
                |> List.filter isSome
                |> List.map unwrapSome
                
                
            let maybeChar = // Some 'a'
                match getSquare coord board with
                | Some sq ->
                    // There is a letter placed on the tile, so we can proceed
                    Some (sq.letter |> fst)
                | _ -> match isSquareAvailable coord squares with
                       | true ->
                           // The square can be played on, but it is currently empty
                           // Lets do the 65 IQ thing and just generate the results based on the first character in the word
                           Some (List.head word |> fst)
                       | false ->
                           // DebugPrint.debugPrint (sprintf "Square was not able to be played at %A, word: %A\n" coord word)
                           None // The square is not able to have words played on it
    
            match maybeChar with
            | Some c ->
                determineCoordinatesWithDuplicates c word coord orientation board squares
                |> List.map (fun coords' -> aux 0 coords' [] [])
                |> List.map (fun (coords', posToRemove) -> { word = removePosFromWord word posToRemove; coordinates = coords'; orientation = orientation })
                |> Some
            | None -> None
            
            
    /// This insertion function absolutely trusts the input, since it assumes that it was ran through
    /// the other functions that determined whether or not you were able to place the given word at the
    /// given coordinates
    let insertWord : (char * int) list -> (int * int) list -> WordOrientation -> StatefulBoard -> StatefulBoard
        = fun word coords wordOrientation (SB (board, letters)) ->
            
            let rec aux pos (board': Dictionary<(int * int), StatefulSquare>) (letters': Dictionary<char, (int * int) list>) word' coords' =
                // DebugPrint.debugPrint (sprintf "insertWord.aux, word: %A\n" word')
                match word', coords' with
                | l :: ls, c :: cs ->
                    // DebugPrint.debugPrint (sprintf "l: %A, ls: %A\n" l ls)
                    // DebugPrint.debugPrint "Matching on list\n"
                    match board'.TryGetValue (c) with
                    | true, placedLetter ->
                        // DebugPrint.debugPrint (sprintf "Board had something placed at %A\n" c)
                        board'.Add(c, {letter = l; word = word; pos = pos; orientation = Both})
                        
                        let coordList = 
                            match letters'.TryGetValue(fst l) with
                            | true, list -> list
                            | false, _ -> []

                        ignore <| if letters'.ContainsKey(fst l) then letters'.Remove(fst l) else false  
                        letters'.Add(fst l, coordList)
                        
                        aux (pos + 1) board' letters' ls cs
                    | false, _ ->
                        // DebugPrint.debugPrint (sprintf "Board didnt contain any thing at %A\n" c)
                        board'.Add(c, {letter = l; word = word; pos = pos; orientation = wordOrientation})
                        
                        // DebugPrint.debugPrint "Attempting to get coord list\n"
                        let coordList = 
                            match letters'.TryGetValue(fst l) with
                            | true, list -> c :: list
                            | false, _ -> [c]

                        // DebugPrint.debugPrint (sprintf "Updaing coordlist: %A -> %A\n" (fst l) coordList)
                        // prevent crashing if key is already in the dict
                        ignore <| if letters'.ContainsKey(fst l) then letters'.Remove(fst l) else false  
                        
                        letters'.Add(fst l, coordList)
                        
                        // DebugPrint.debugPrint "Going for the recursive call now\n"
                        aux (pos + 1) board' letters' ls cs
                | _, _ -> SB (board', letters')
            
            aux 0 board letters word coords
            
    
    
  
      
