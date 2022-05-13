namespace EmmaGaddagBot

module internal WordSearch =
    
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
        availableLetters |> List.filter (fun c -> c = char 0 || contains (letterToNumber c) hand || MultiSet.contains 0u hand)

    let toCorrectWord : (char * int) list -> (char * int) list =
        fun word ->
            List.findIndex (fst >> (=) (char 0)) word 
            |> (fun i -> (List.take i word |> List.rev) @ (List.skip (i + 1) word))

    let rec traverseDictForWords (c: char) (hand: MultiSet.MultiSet<uint32>) (dict: ScrabbleUtil.Dictionary.Dict) (currentWord : (char * int) list) (foundWords: HashSet<(char * int) list>) : HashSet<(char * int) list> =
        match size hand with
        | 0u -> foundWords
        | _ ->
            match step c dict with
            | Some (isWord, dict') ->
                let hand', wildcardWasUsed =
                    if contains (letterToNumber c) hand
                    then (removeSingle (letterToNumber c) hand, false)
                    else (removeSingle 0u hand, true)
                
                let currentWord' =
                    if wildcardWasUsed
                    then List.append currentWord [(c, 0)]
                    else List.append currentWord [(Utils.pairLetterWithPoint c)]

                // Update the found words and ignore the output of this, it is not relevant
                ignore <| if isWord then foundWords.Add(currentWord') else true

                nextLettersInGaddag dict 
                |> actuallyAvailableLetters hand'
                |> List.map (fun c -> traverseDictForWords c hand' dict' currentWord' foundWords)
                |> List.fold (fun acc hs -> acc.UnionWith(hs); acc) (new HashSet<(char*int) list>())

            | None ->
                // The edge long char 'c' does not lead to a new node
                foundWords

    let findCandidateWords (hand: MultiSet.MultiSet<uint32>) (dict: ScrabbleUtil.Dictionary.Dict) (c :char): (char * int) list list
        = traverseDictForWords c hand dict [] (new HashSet<(char * int) list>())
          |> fun words -> [for word in words -> toCorrectWord word]
          
          
          
        
