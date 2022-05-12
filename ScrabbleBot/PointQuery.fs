namespace EmmaGaddagBot
open System.Collections.Generic

module PointQuery =
    
    type Protocol =
        | Put of int * int
        | Get of AsyncReplyChannel<(int * int) option>
    
    let mutable queue = Queue<int * int>()

    let postbox =
        let aux (inbox: MailboxProcessor<Protocol>) =
            let rec loop () = async {
                let! msg = inbox.Receive()
                match msg with
                | Put (x, y) -> queue.Enqueue((x, y))
                | Get ch ->
                    match queue.TryDequeue() with
                    | true, coord -> ch.Reply(Some coord)
                    | false, _ -> ch.Reply(None)
                return! loop ()
            }
            loop()
        
        MailboxProcessor.Start(aux)
    
    let get : unit -> (int * int) option
        = fun () ->
            postbox.PostAndReply(Get)
        
    let put : (int * int) -> unit
        = fun coord -> postbox.Post(Put coord)


