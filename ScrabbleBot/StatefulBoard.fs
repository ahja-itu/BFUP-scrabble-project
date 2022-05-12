namespace EmmaGaddagBot


open System.Collections.Generic
open Parser
open ScrabbleUtil
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
    type StatefulBoard = SB of Dictionary<(int * int), StatefulSquare>
    
    let mkStatefulBoard () =
        SB (Dictionary<(int * int), StatefulSquare>())
        
    let getSquare (coords: (int * int)) (SB board: StatefulBoard) : StatefulSquare option = 
        match board.TryGetValue(coords) with
        | true, statefulSquare -> Some statefulSquare
        | false, _ -> None
        
    let positionIsNotWithBothWordDirections : StatefulBoard -> int * int -> bool
        = fun sb coords  ->
            let (SB board) = sb
            match getSquare coords sb with
            | Some sq -> not (sq.orientation = Both)
            | None -> false
           
           
    // TODO: make this function removable
    let getPlacedTilesAndPositons (sb: StatefulBoard) : (char * (int * int) list) list =
        let (SB board) = sb
        [for key in letters.Keys -> (key, letters.[key])]
        |> List.map(fun (c, lst) -> (c, List.filter (positionIsNotWithBothWordDirections sb) lst))

    
    // TODO make this function removable
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
        | StateMonad.Success maybeSquare ->
            match maybeSquare with
            | Some _ -> true
            | None -> false
        | _ -> false
            
    let hasSufficientSpaceAround : int * int -> StatefulBoard -> bool
        = fun (x, y) sb ->
            let surroundingSquares = [(x, y - 1); (x, y + 1); (x - 1, y); (x + 1, y)]

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
    
    let isSingleSquarePlaceable (coords: (int * int)) (statefulBoard: StatefulBoard) (squares: boardFun2) : bool =
        // A square is "placeable" if it is a valid square in the board program, it doesn't have any tiles on it
        // in our own stateful board
        match (getSquare coords statefulBoard) with  
            | None -> true
            | _ -> false
        |> (&&) (isSquareAvailable coords squares)
    
    let allowNeighboringPositions : int * int -> int * int -> StatefulBoard -> boardFun2 -> bool
        = fun (x, y) (x', y') sb squares ->
            let (a, b) = (x - x', y - y')
            let domain = [-1; 1]
            
            let coordsToCheck =
                [(x', y' - 1); (x', y' + 1); (x' - 1, y'); (x' + 1, y')]
                |> List.filter (fun c -> not (c = (x, y)))
            
            (List.contains a domain <> List.contains b domain) && List.forall (fun coord -> isSingleSquarePlaceable coord sb squares) coordsToCheck
    
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
                (isSquarePlaceable coordSet sb squares || 
                allowNeighboringPositions (x, y) coordSet sb squares))) // If all remaining coordinates are free to have tiles placed there, then its good
        
    let possibleWordPlacements : int * int -> (char * int) list -> WordOrientation -> StatefulBoard -> boardFun2 -> WordInsertPayload list option
        = fun coord word orientation board squares ->
            
            // TODO: we can determine which placed tiles that should be of orientation both in this function
            //       and we are able to update the board from here, so it would be nice to do
            
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
                
            // TODO: We can simplify this a lot - we know this coord to have something since it comes from the letters dict
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
    let determineDirectionOfPlayedWord (ms: (coord * (uint32 * (char * int))) list) (sb: StatefulBoard) : WordOrientation =
        match ms with
        | [] -> DebugPrint.debugPrint "determineDirectionOfPlayedWord failed since 'ms' was empty"; failwith "why u ignore dis"
        | [((x, y), _)] -> // Lets check around the placed tile and see what is there to determine the orientation
            // We have 4 cases:
            let neighbouringCoords = [(x - 1, y); (x + 1, y); (x, y + 1); (x, y - 1)]
            List.fold (fun acc x ->
                match getSquare x sb with
                | Some sq -> Some (oppositeOrientation sq.orientation)
                | None -> acc
                ) None neighbouringCoords
            |> fun orientation ->
                match orientation with
                | Some orientation' -> orientation'
                | _ -> DebugPrint.debugPrint (sprintf "Single tile placed at %A had no neightbors. Should not happen" (x, y)); failwith "lol"
        | _ -> 
            match ms.[0], ms.[1] with
            | ((x1, _), _), ((x2, _), _) ->
                if x1 = x2 then Vertical else Horizontal
            
    /// This insertion function absolutely trusts the input, since it assumes that it was ran through
    /// the other functions that determined whether or not you were able to place the given word at the
    /// given coordinates
    let insertWord : (char * int) list -> (int * int) list -> WordOrientation -> StatefulBoard -> StatefulBoard
        = fun word coords wordOrientation (SB board) ->
            let rec aux pos (board': Dictionary<(int * int), StatefulSquare>) word' coords' =
                match word', coords' with
                | l :: ls, c :: cs ->
                    match board'.TryGetValue (c) with
                    | true, placedLetter ->
                        board'.Add(c, {letter = l; word = word; pos = pos; orientation = Both})
                        aux (pos + 1) board' ls cs
                    | false, _ ->
                        // Update the board
                        board'.Add(c, {letter = l; word = word; pos = pos; orientation = wordOrientation})
                        // Let the coordinate be playable for another word across
                        PointQuery.put(c)
                        aux (pos + 1) board' ls cs
                | _, _ -> SB board'
            
            aux 0 board word coords
    
    
    
    let playFromWord (sb : StatefulBoard) (squares: boardFun2) (hand: MultiSet.MultiSet<uint32>) (candidateWord: string) : ((int * int) * (uint32 * (char * int))) list = 
                let word = candidateWord.ToCharArray() |> Array.toList |> List.map (Utils.pairLetterWithPoint) |> Seq.toList
         
                // We find the "bogus" character in the word that we don't have on the hand
                let _, charToPlayFrom =
                    candidateWord |> Seq.fold (fun (hand', a) c ->
                        if MultiSet.contains (Utils.letterToNumber c) hand'
                        then (MultiSet.removeSingle (Utils.letterToNumber c) hand', a)
                        else (hand', c)) (hand, char 0)
                    
                // We get the coordinate sof all places where that character is located on the board
                let coords = getPlacedTilesAndPositonsForChar charToPlayFrom sb
                
                
                let isNotBothWordDirections coords =
                    match getSquare coords sb with
                    | Some sq -> not (sq.orientation = Both)
                    | _ -> true
                    
                let getOppositeDirection coords =
                    match getSquare coords sb with
                    | Some sq -> oppositeOrientation sq.orientation
                    | _ -> Horizontal
                
                let rec findFirstPossibleWordPlay (rootCharsToPlayFrom: (int * int) list) =
                    match rootCharsToPlayFrom with
                    | [] -> None
                    | coord :: coords ->
                        // debugPrint (sprintf "Considering plays for %c\n" c)
                        
                        let possiblePlacements =
                            // TODO This check might be redundant
                            if isNotBothWordDirections coord
                            then possibleWordPlacements coord word (getOppositeDirection coord) sb squares
                            else None
                        
                        match possiblePlacements with
                        | Some (x :: _) -> Some x
                        | _ -> findFirstPossibleWordPlay coords

                // debugPrint "Going to find first possible word play\n"
                let playPayload = findFirstPossibleWordPlay coords
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
    
  
      
