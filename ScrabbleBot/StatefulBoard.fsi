namespace EmmaGaddagBot

module StatefulBoard =

    open ScrabbleUtil.ServerCommunication

    type WordOrientation = Horizontal | Vertical
    type StatefulSquare = {
        word: (char * int) list         // All characters in the word, and the associated point value
        pos : int                       // The position in the word in which the letter is located
        letter: (char * int)            // Character and point
        orientation : WordOrientation   // The orientation of the word, in respect to the board
    }
    type StatefulBoard


    val mkStatefulBoard : unit -> StatefulBoard

    val getSquare : (int * int) -> StatefulBoard -> StatefulSquare option // x,y-coordinate -> current board -> updated board


    val getPlacedTilesAndPositons : StatefulBoard -> (char * (int * int) list) list
    val getPlacedTilesAndPositonsForChar : char -> StatefulBoard -> (int * int) list

    // checks for collition for a word, to be put around a position, the board itself and it might 
    // return a coordinate where to put the first letter of the word and in which orientation it needs to be inserted
    val checkCollisions : (char * int) list -> (int * int) -> StatefulBoard -> ((int * int) * WordOrientation) option


    val insertWord : (int * int) -> WordOrientation -> (char * int) list -> StatefulBoard -> Result<StatefulBoard, GameplayError> // x,y starting coordinate -> horisontal/vertical -> current borad -> updated board
    
    val playWord : (int * int) -> WordOrientation -> words: (char * int) list list -> StatefulBoard -> (Result<StatefulBoard, GameplayError> * (char *int) list * (int * int) list)


    (*

        Thoughts and observations:

        - For each letter, there will be one or more "best" words to play
          - The best word to play from a letter on the board MIGHT not have the space needed to put the whole word down
          - We can query the Gaddag to get candidates of words to play
          - Ways to check if we can play a word at a certain position:
            - Is there enough space to accomodate the word on both sides of the placed tile




    *)