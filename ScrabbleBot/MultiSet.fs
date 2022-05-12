module internal MultiSet
    type MultiSet<'a when 'a : comparison> = MS of Map<'a, uint32>

    let unpack (MS ms) = ms
    let safeDecrease a b = if a <= b then 0u else a - b 
    
    let empty = MS Map.empty<'a, uint32>
    let size (MS ms) = Map.fold (fun acc k v -> acc + v) 0u ms
    let isEmpty mms = size mms = 0u
    let numItems a (MS ms) = ms.TryFind a |> Option.defaultValue 0u
    let contains a (MS ms) = ms.ContainsKey a && (numItems a (MS ms) > 0u)
    let add a n mms =
        numItems a mms 
        |> (fun count -> (unpack mms).Add (a, n + count))
        |> MS
    
    let addSingle a mms = add a 1u mms
    
    let remove a n mms =
        numItems a mms
        |> (fun count ->
            let newVal = safeDecrease count n
            in if newVal = 0u 
               then (unpack mms).Remove(a) 
               else (unpack mms).Add (a, (newVal)))
        |> MS
        
    let removeSingle a mms = remove a 1u mms
        
    let fold: ('a -> 'b -> uint32 -> 'a) -> 'a -> MultiSet<'b> -> 'a 
        = fun f acc (MS ms) -> Map.fold f acc ms
        
    let foldBack : ('a -> uint32 -> 'b -> 'b) -> MultiSet<'a> -> 'b -> 'b
        = fun f (MS ms) acc -> Map.foldBack f ms acc
        
    let ofList : 'a list -> MultiSet<'a>
        = fun lst ->
            List.fold (fun acc t -> addSingle t acc) empty lst
    
    let toList : MultiSet<'a> -> 'a list
        = fun mms ->
            fold (fun acc k v -> List.replicate (int v) k @ acc) [] mms
            |> List.rev
            
    let map : ('a -> 'b) -> MultiSet<'a> -> MultiSet<'b>
        = fun f mms ->
            fold (fun acc k v -> add (f k) v acc) empty mms
            
    // We're going to abandon names s1 and s2 and embrace dataset circles naming (left & right)s
    let union : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
        = fun left right ->
            fold (fun union k v -> if numItems k left < v then add k v union else union) left right
            
    let sum : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
        = fun left right ->
            fold (fun sum k v -> add k ((numItems k left) + v) sum) empty right
            
    let subtract : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
        = fun left right ->
            fold (fun difference k v -> remove k v difference) left right
    
    let intersection : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
        = fun left right ->
            
            let aux middle k v =
                let aux' rightN leftN =
                    match (rightN, leftN) with
                    | 0u, _ -> middle
                    | _, 0u -> middle
                    | r, l -> add k (min r l) middle
                aux' (numItems k right) v
            
            fold aux empty left
        