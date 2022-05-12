namespace EmmaGaddagBot

open Eval
open MultiSet
open Parser

module internal StatefulBoard =

    type WordOrientation = Horizontal | Vertical | Both
    type StatefulSquare = {
        word: (char * int) list         // All characters in the word, and the associated point value
        pos : int                       // The position in the word in which the letter is located
        letter: (char * int)            // Character and point
        orientation : WordOrientation   // The orientation of the word, in respect to the board
    }
    type StatefulBoard
    
    type WordInsertPayload = {
        word: (char * int) list
        coordinates: (int * int) list
        orientation: WordOrientation
    }


    val oppositeOrientation : WordOrientation -> WordOrientation
    val mkStatefulBoard : unit -> StatefulBoard

    val getSquare : (int * int) -> StatefulBoard -> StatefulSquare option // x,y-coordinate -> current board -> updated board
    
    val determineCoordinatesWithDuplicates : char -> (char * int) list -> int * int -> WordOrientation -> StatefulBoard -> boardFun2 -> (int * int) list list
    val possibleWordPlacements : int * int -> (char * int) list -> WordOrientation -> StatefulBoard -> boardFun2 -> WordInsertPayload list option
   
    val insertWord : (char * int) list -> (int * int) list -> WordOrientation -> StatefulBoard -> StatefulBoard
    
    val playFromWord : StatefulBoard -> boardFun2 -> MultiSet<uint32> -> (char * (int * int)) -> string -> ((int * int) * (uint32 * (char * int))) list
    val determineDirectionOfPlayedWord : (coord * (uint32 * (char * int))) list -> StatefulBoard -> WordOrientation