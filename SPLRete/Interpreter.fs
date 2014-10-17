namespace SPLRete

module Interpreter =
  open PatternMatching.ReteNetwork
  open PatternMatching.ReteInterpreter
  open PatternMatching.PatternTree
  open SimpleProductionLanguage.AST

  let evalOp =
      function | Plus -> (+) | Minus -> (-) | Times -> (*) | Division -> (/)

  let evalExp (lvals : LValue list, binding: WME list) e =
    let rec loop =
      function
      | Constant i -> Int i
      | Deref lval ->
        let index = List.findIndex ((=)lval) lvals
        let _, values = List.nth binding index
        let targetWMEOffset =
          match lval with
          | Proj _ -> 2
          | LValue.Variable _ -> 0
        Array.get values targetWMEOffset
      | BinOp(e1,op,e2) ->
          let i1 = getInt <| loop e1
          let i2 = getInt <| loop e2
          Int <| evalOp op i1 i2
    loop e

  type Interpreter(reteGraph : ReteGraph<LValue list * Action>) =
    let facts = ref Set.empty

    let getFacts () = !facts

    let getInstances() =
      Seq.choose
          (function
          | "class",[|Int instId; String _|] -> Some instId
          | _ -> None) <| getFacts()

    let findInstancesByType iType =
      Seq.choose
          (function
          | "class",[|Int instId; String iType'|] when iType = iType' -> Some instId
          | _ -> None) <| getFacts()

    let findAssignments instId =
      Seq.choose (function ("assign",[|Int instId'; String var; value|]) when instId' = instId -> Some(var, value) | _ -> None ) <| getFacts()

    let findInstances iType assignments =
      let facts = getFacts()
      let instFilter instId =
          Seq.forall (fun (var, value) -> Set.contains ("assign",[|Int instId; String var; value|]) facts) assignments
      Seq.filter instFilter <| findInstancesByType iType

    let getMaxInstanceId() = Seq.max << Seq.append (Seq.singleton 0) <| getInstances()

    let evalAction env : Action -> seq<Fact> =
      function
      | FindOrCreate(instType, assignments) ->
        let evaluatedAssignments = List.map (fun(var, exp) -> var, evalExp env exp) assignments
        let instances = Array.ofSeq <| findInstances instType evaluatedAssignments
        let evalStr (var, value) = sprintf "%s = %A"  var value
        match Seq.length instances with
        | 0 ->
          let instId = getMaxInstanceId () + 1
          let fact : Fact = "class", [|Int instId; String instType|]
          Seq.ofList <| fact :: (List.map (fun(var, value) -> "assign", [|Int instId;String var; value|]) evaluatedAssignments)
        | 1 -> Seq.empty
        | _ -> failwithf "multi match %A %s(%s)" instances instType (String.concat ", " <| Seq.map evalStr evaluatedAssignments)

    let evalConflict ((lvals : LValue list, action), wmes : WME list) =
      let env = lvals, wmes
      evalAction env action

    let rec activateEvalSet (inputFacts:Set<Fact>) =
      if Set.isEmpty inputFacts
      then ()
      else
        let conflictSet = Set.ofSeq <| Seq.collect (activate reteGraph) inputFacts
        facts := Set.unionMany[ !facts; inputFacts]
        let update conflict =
          let newFacts = evalConflict conflict
          facts := Set.unionMany[ !facts; Set.ofSeq <| newFacts]
          newFacts
        let newFacts = Set.ofSeq <| Seq.collect update conflictSet
        activateEvalSet newFacts

    let activateEval fact = activateEvalSet <| Set.singleton fact

    member __.GetFacts() = getFacts()
    member __.GetMaxInstId() = getMaxInstanceId()
    member __.GetInstancesOfType iType = findInstancesByType iType
    member __.GetAssignments instId = findAssignments instId

    member __.Add(fact:Fact) =
      if Set.contains fact !facts
      then failwithf "Fact already added %A" fact
      activateEval fact

//    member __.Remove(fact:Fact) =
//      if not <| Set.contains fact !userFacts
//      then failwith "Could not remove fact not added"
//      failwith ""
