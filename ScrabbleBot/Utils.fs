namespace EmmaGaddagBot

open System.Runtime.InteropServices
open Parser

module internal Utils =

    open ScrabbleUtil

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

    
    let chooseHighestScoringWord : (char * int) list -> (char * int) list -> (char * int) list
        = fun a b ->
            let countPoints lst =
                List.map snd lst |> List.fold (+) 0
            if countPoints a < countPoints b then b else a