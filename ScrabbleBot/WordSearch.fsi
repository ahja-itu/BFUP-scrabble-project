namespace EmmaGaddagBot

module internal WordSearch =

    val findCandidateWords : MultiSet.MultiSet<uint32> -> ScrabbleUtil.Dictionary.Dict -> char -> (char * int) list list
