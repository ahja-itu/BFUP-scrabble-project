namespace EmmaGaddagBot

module internal WordSearch =

    val findCandidateWords : char -> MultiSet.MultiSet<uint32> -> ScrabbleUtil.Dictionary.Dict -> string list
