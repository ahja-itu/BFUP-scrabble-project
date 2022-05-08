namespace EmmaGaddagBot

module internal Utils =

    open MultiSet

    let numberToLetter : uint32 -> char =
        fun (n: uint32) -> char (n + 64u)
    
    let letterToNumber : char -> uint32 =
        fun c -> (uint32 c) - 64u

    let shortestStringOf a b =
                if String.length a < String.length b then b else a
    
    let handToLetters : MultiSet.MultiSet<uint32> -> char array =
        fun (hand) ->
            MultiSet.toList hand |> List.map numberToLetter |> List.toArray


    let pairLetterWithPoint (letter: string) : (string * int)  =
        match letter with
        | "A"|"E"| "I"| "O"| "U"| "L"| "N"| "S"| "T"| "R" -> 1
        | "D"| "G" -> 2
        | "B"| "C"| "M"| "P" -> 3
        | "F"| "H"| "V"| "W"| "Y" -> 4
        | "K" -> 5
        | "J"| "X" -> 8
        | "Q"| "Z" -> 10
        | _ -> failwith "Letter looked up was outside of the alphabet (this should not happen!)"
        |> fun p -> (letter, p)

