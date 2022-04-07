module Dictionary
    open System
    open System.Collections
    
    (* Type declarations *)
    type Dictionary =
        // Leaf of bool
        | Node of int * bool * Generic.Dictionary<char, Dictionary>
    
    let mutable n = 0
    
    (* Helper functions: *)
    let stop = char 0
    let stringToCharacterList : string -> char list
        = Seq.toList >> List.map (char)
        
    let reverseUntilPosition : int -> char list -> char list
        = fun n lst ->
            match n with
            | 1 -> lst
            | _ when n = List.length lst -> List.rev lst
            | _ when n > List.length lst -> failwith "n was larger than list length"
            | _ -> ((List.take n >> List.rev) lst) @ (List.skip n lst)
    
//    let rec listInsertAt : int -> 'a -> 'a list -> 'a list
//        = fun pos x xs ->
//            match pos, xs with
//            | 0, [] -> [x]
//            | _, [] -> []
//            | 0, _ -> x :: xs
//            | _, h :: t -> h :: listInsertAt (pos - 1) x t
    
    let wordPermutations : string -> char list list
        = fun input ->
            let asCharacterList = stringToCharacterList input
            [for pos in [1 .. input.Length] do
                 asCharacterList
                 |> List.insertAt pos stop // This will not work in CodeJudge
                 // |> listInsertAt pos stop // This is for CodeJugde, remove later I guess
                 |> reverseUntilPosition pos ]
    
    (* Implemented signature functions *)
    let empty : unit -> Dictionary
        = fun () -> Node (0, false, Generic.Dictionary<char, Dictionary>())
        
    let emptyWithLevel (n: int) :  Dictionary
        = Node (n + 1, false, Generic.Dictionary<char, Dictionary>())
   
    let emptyMap = Map.empty<char, Dictionary>
    let emptyNode lastLevel = emptyWithLevel lastLevel
    
    let insert : string -> Dictionary -> Dictionary
        = fun word dict ->
            let rec aux (Node (level, isWord, subtreeMap)) word' =
                match word' with
                | [] -> Node (level, true, subtreeMap)
                | x :: xs ->
                    let childNode = match subtreeMap.TryGetValue(x) with
                                    | false, _ -> emptyNode level
                                    | true, subtreeMap' -> subtreeMap'
                    subtreeMap.[x] <- aux childNode xs
                    
                    Node (level, isWord, subtreeMap)
            
            wordPermutations word |> List.fold aux dict

    
    let dictGet : char -> Dictionary -> (bool * Dictionary) option
        = fun c (Node (level, _, dict)) ->
            match dict.TryGetValue(c) with
            | false, _ -> None
            | true, dict' -> match dict' with
                               | Node (_, isWord', _) -> Some (isWord', dict')

    
    let step : char -> Dictionary -> (bool * Dictionary) option
        = fun c dict -> dictGet c dict
            
    let reverse : Dictionary -> (bool * Dictionary) option
        = fun dict -> dictGet stop dict
        
    let isWord (Node (_, b, _)) = b
   
    let lookup : string -> Dictionary -> bool
        = fun word dict ->
            let rec aux (word': char list) (dict': Dictionary) : bool =
                match word' with
                | [] -> isWord dict'
                | x :: xs -> match step x dict' with
                                    | Some (isWord2, dict'') -> aux xs dict''
                                    | None -> match reverse dict' with
                                              | Some (isWord3, dict'') -> aux (x :: xs) dict''
                                              | None -> false
            
            wordPermutations word
            |> (fun lst -> if lst.Length = 0 then true else aux lst.[0] dict)
            
            
