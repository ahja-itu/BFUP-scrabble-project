namespace EmmaGaddagBot

module internal PointQuery =
    
    type Protocol =
        | Put of int * int
        | Get of AsyncReplyChannel<(int * int) option>
    
    val get : unit -> (int * int) option
    val put : (int * int) -> unit
    val resetAfterRound : unit -> unit
    
    val print : unit -> unit
    
    

