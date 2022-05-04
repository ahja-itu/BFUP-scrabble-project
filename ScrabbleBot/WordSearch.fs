namespace EmmaGaddagBot

module internal WordSearch =


    (*

        TODO: all words as char lists, such that we can represent the special character from the Gaddag
              and then finally correct the found words into strings that are "ordered" correctly

    *)

    open ScrabbleUtil.Dictionary
    open MultiSet
    open System.Collections.Generic

    /// Assumes that the input represents an upper case letter (ASCII Code starting from 65 at least)
    let numberToLetter : uint32 -> char =
        fun (n: uint32) -> char (n + 64u)
    
    let letterToNumber : char -> uint32 =
        fun c -> (uint32 c) - 64u

    /// This is the alphabet for the Scrabble game, including the special character for the Gaddag
    let alphabet = char 0 :: ['A'..'Z']

    /// Computes the letters available to move down in the Gaddag
    let nextLettersInGaddag : ScrabbleUtil.Dictionary.Dict -> char list =
        fun (dict) ->
            let aux (dict: ScrabbleUtil.Dictionary.Dict) (c: char) =
                match step c dict with
                | Some _ -> true
                | None -> false

            alphabet |> List.filter (aux dict)

    let actuallyAvailableLetters (hand: MultiSet.MultiSet<uint32>) (availableLetters: char list) : char list =
        availableLetters |> List.filter (fun c -> c = char 0 || contains (letterToNumber c) hand)

    let toCorrectWord : char list -> string =
        fun word ->
            List.findIndex ((=) (char 0)) word 
            |> (fun i -> (List.take i word |> List.rev) @ (List.skip (i + 1) word))
            |> (fun s -> List.toSeq s |> string)

    let rec traverseDictForWords (c: char) (hand: MultiSet.MultiSet<uint32>) (dict: ScrabbleUtil.Dictionary.Dict) (currentWord : char list) (foundWords: HashSet<char list>) : HashSet<char list> =
        match size hand with
        | 0u -> foundWords
        | _ -> 
            match step c dict with
            | Some (isWord, dict') ->
                // We have found a complete word!
                let hand' = removeSingle (letterToNumber c) hand
                let currentWord' = List.append currentWord [c]

                // Update the found words and ignore the output of this, it is not relevant
                ignore <| if isWord then foundWords.Add(currentWord') else true

                nextLettersInGaddag dict 
                |> actuallyAvailableLetters hand'
                |> List.map (fun c -> traverseDictForWords c hand' dict' currentWord' foundWords)
                |> List.fold (fun acc hs -> acc.UnionWith(hs); acc) (new HashSet<char list>())

            | None ->
                // The edge long char 'c' does not lead to a new node
                foundWords

    let findCandidateWords (c: char) (hand: MultiSet.MultiSet<uint32>) (dict: ScrabbleUtil.Dictionary.Dict) : string list
        = traverseDictForWords c hand dict [] (new HashSet<char list>())
          |> fun words -> [for word in words -> toCorrectWord word]
          
          
