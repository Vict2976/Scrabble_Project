// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a: comparison> = Map<'a, uint32>

    let empty = Map.empty<'a, uint32>
    
    let isEmpty (input : MultiSet<'a>) = Map.isEmpty input
    
    let size (input : MultiSet<'a>) = Map.fold(fun acc k v -> acc + v) 0u input
    
    let contains (a : 'a) (input : MultiSet<'a>) = Map.containsKey a input
    
    let numItems (a : 'a) (input : MultiSet<'a>) = try Map.find a input with _ -> 0u
    
    let add key value (input : MultiSet<'a>) =
        match Map.tryFind key input with
        | Some p -> Map.add key (value + p) input
        | None -> Map.add key value input
    
    let addSingle key (input : MultiSet<'a>) = //Evt bare kald add key 1u input
        match Map.tryFind key input with
        | Some p -> Map.add key (1u + p) input
        | None -> Map.add key 1u input
    
    let remove key value (input : MultiSet<'a>) =
        match Map.tryFind key input with
        | Some p -> if value < p then Map.add key (p-value) input else Map.remove key input
        | None -> input
    
    let removeSingle key (input : MultiSet<'a>) =
        match Map.tryFind key input with
        | Some p -> if 1u < p then Map.add key (p-1u) input else Map.remove key input
        | None -> input
    
    let fold func acc (input : MultiSet<'a>) = Map.fold func acc input
    
    let foldBack func (input : MultiSet<'a>) acc = Map.foldBack func input acc
