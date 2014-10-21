namespace PatternMatching

module PatternDiscriminatorTree =
  open PatternMatching.PatternTree

  type PatternDiscriminatorTree<'value> =
    PTreeAny of 'value option * PatternDiscriminatorTree<'value> option * Map<Value, PatternDiscriminatorTree<'value>>

  let empty = PTreeAny(None, None, Map.empty)

  let optDefault opt defaultVal =
    match opt with
    | Some v -> v
    | None -> defaultVal

  let addPatternArray (pattern:Pattern) value tree =
    let rec loop i (PTreeAny(valueOpt, anythingOpt, childMap)) =
      if i = pattern.Length
      then PTreeAny(Some value, anythingOpt, childMap)
      else
        let keyVal = Array.get pattern i
        match keyVal with
        | Anything _ ->
          PTreeAny(valueOpt, Some <| insertOption anythingOpt i, childMap)
        | PatternValue pv ->
          let child = Map.tryFind pv childMap
          let newTree = insertOption child i
          PTreeAny(valueOpt, anythingOpt, Map.add pv newTree childMap)
    and insertOption opt i = loop (i + 1) <| optDefault opt empty
    loop 0 tree

  let tryFindPattern (patternValues : ValuePattern array) (tree:PatternDiscriminatorTree<'value>) : 'value option =
    let rec loop i node =
      match node with
      | PTreeAny(valueOpt, anythingOpt, childrenMap) ->
        if i = patternValues.Length
        then valueOpt
        else
          if i > patternValues.Length
          then
            None
          else
            let patternValue = Array.get patternValues i
            let childNode =
              match patternValue with
              | Anything _ -> anythingOpt
              | PatternValue value -> Map.tryFind value childrenMap
            Option.bind (fun anything -> loop(i+1) anything) childNode
    loop 0 tree

  let rec allValues (PTreeAny(valueOpt, anythingOpt, childrenMap)) : seq<_> =
    Seq.concat [Option.toArray valueOpt :> seq<_>
                optDefault (Option.map allValues anythingOpt) Seq.empty
                Seq.collect allValues << Seq.map snd <| Map.toSeq childrenMap
                ]

  let rec map f (PTreeAny(valueOpt, anythingOpt, childrenMap)) =
    PTreeAny(Option.map f valueOpt, Option.map (map f) anythingOpt, Map.map (fun _ v -> map f v) childrenMap)

  let tryFindValues (values : Value array) tree =
    let rec loop i node =
      match node with
      | PTreeAny(valueOpt, anythingOpt, childrenMap) ->
        if i = values.Length
        then Option.toArray valueOpt :> seq<_>
        else
          if i > values.Length
          then
            Seq.empty
          else
            let keyVal = Array.get values i
            let child = Map.tryFind keyVal childrenMap
            let children = Array.append (Option.toArray anythingOpt) (Option.toArray child)
            Seq.collect (loop (i + 1)) children
    loop 0 tree
