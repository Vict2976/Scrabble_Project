// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type 'a MultiSet when 'a: comparison = MS of Map<'a, uint32>
    

    let empty = MS (Map.empty)
    let isEmpty (MS s) = Map.isEmpty s
    let size (MS s) = Map.fold (fun acc key value -> acc + value) 0u s
    
    let contains a (MS s) =
        match Map.tryFind a s with
        | Some value -> true
        | None -> false
       
       
    let numItems a (MS s) =
        match Map.tryFind a s with
        | Some value -> value
        | None -> 0u
       
    
    let add a n (MS s) =
        match Map.tryFind a s with
        | Some value -> Map.add a (n+value) s
        | None -> Map.add a n s
        |> MS 
    
    let addSingle a (MS s) =
        match Map.tryFind a s with
        | Some value -> Map.add a (value+1u) s
        | None -> Map.add a 1u s
        |> MS
    
    
    let remove a n (MS s) =
        match Map.tryFind a s with
        | Some value -> if (value <= n) then Map.remove a s
                        else Map.add a (value - n) s
        | None -> s
        |> MS
    
    let removeSingle a (MS s) =
        match Map.tryFind a s with
        | Some value -> if (value-1u)< 0u then Map.add a 0u s
                        else Map.add a (value-1u) s
        | None -> s
        |> MS
        
    let fold f acc (MS s) = Map.fold f acc s
    
    let foldBack f (MS s) acc = Map.foldBack f s acc