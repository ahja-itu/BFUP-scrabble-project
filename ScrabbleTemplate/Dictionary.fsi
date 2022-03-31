module Dictionary
    type Dictionary
    
    val empty : unit -> Dictionary
    val insert : string -> Dictionary -> Dictionary
    val step : char -> Dictionary -> (bool * Dictionary) option
    val reverse : Dictionary -> (bool * Dictionary) option
    val lookup : string -> Dictionary -> bool
    
    