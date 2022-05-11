namespace EmmaGaddagBot

open System.Runtime.InteropServices
open Parser

module internal Utils =

    open ScrabbleUtil
    open StatefulBoard

    let numberToLetter : uint32 -> char =
        fun (n: uint32) -> char (n + 64u)
    
    let letterToNumber : char -> uint32 =
        fun c -> (uint32 c) - 64u

    let shortestStringOf a b =
                if String.length a < String.length b then b else a
    
    let handToLetters : MultiSet.MultiSet<uint32> -> char array =
        fun (hand) ->
            MultiSet.toList hand |> List.map numberToLetter |> List.toArray

    let pairLetterWithPoint (letter: char) : (char * int)  =
        match letter with
        | 'A'|'E'| 'I'| 'O'| 'U'| 'L'| 'N'| 'S'| 'T'| 'R' -> 1
        | 'D'| 'G' -> 2
        | 'B'| 'C'| 'M'| 'P' -> 3
        | 'F'| 'H'| 'V'| 'W'| 'Y' -> 4
        | 'K' -> 5
        | 'J'| 'X' -> 8
        | 'Q'| 'Z' -> 10
        | c -> DebugPrint.debugPrint (sprintf "Failed to convert letter to point: %A\n" c); failwith "Letter looked up was outside of the alphabet (this should not happen!)"
        |> fun p -> (letter, p)

    let determineDirectionOfPlayedWord (ms: (coord * (uint32 * (char * int))) list) (sb: StatefulBoard) : StatefulBoard.WordOrientation =
        match ms with
        | [] -> DebugPrint.debugPrint "determineDirectionOfPlayedWord failed since 'ms' was empty"; failwith "why u ignore dis"
        | [((x, y), _)] -> // Lets check around the placed tile and see what is there to determine the orientation
            // We have 4 cases:
            let neighbouringCoords = [(x - 1, y); (x + 1, y); (x, y + 1); (x, y - 1)]
            List.fold (fun acc x ->
                match StatefulBoard.getSquare x sb with
                | Some sq -> Some (StatefulBoard.oppositeOrientation sq.orientation)
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
