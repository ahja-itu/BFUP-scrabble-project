namespace EmmaGaddagBot
open System.Collections.Generic
open System.Linq

open ScrabbleUtil

module PointQuery =
    
    type Protocol =
        | Put of int * int
        | Get of AsyncReplyChannel<(int * int) option>
    
    let mutable queue = Queue<int * int>()
    let mutable holdQueue = Queue<int * int>()

    let postbox =
        let aux (inbox: MailboxProcessor<Protocol>) =
            let rec loop () = async {
                let! msg = inbox.Receive()
                match msg with
                | Put (x, y) ->
                    
                    if queue.Contains((x, y)) then
                        queue <- Queue<(int * int)>(queue.Where(fun (x', y') -> not ((x, y) = (x', y'))))
                    else queue.Enqueue((x, y))
                | Get ch ->
                    match queue.TryDequeue() with
                    | true, coord ->    
                        holdQueue.Enqueue(coord)
                        ch.Reply(Some coord)
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

    let resetAfterRound () =
        queue <- holdQueue
        holdQueue <- Queue<(int * int)>()
        ()
    
    let print : unit -> unit
        = fun () ->
            DebugPrint.debugPrint "##################################"            
            DebugPrint.debugPrint "Printing PointQuery queue contents:\n"
            for item in queue.ToArray() do
                DebugPrint.debugPrint (sprintf "%A\n" item)
            DebugPrint.debugPrint "##################################"

