namespace SPLRete

module Interpreter =
  open PatternMatching.ReteNetwork
  open PatternMatching.PatternTreeInterpreter
  open PatternMatching.ReteInterpreter
  open PatternMatching.PatternTree
  open SimpleProductionLanguage.AST

  let evalExp (lvals : LValue list, binding: Environment) e =
    let rec loop =
      function
      | Constant i -> Int i
      | Deref lval ->
        let index = List.findIndex ((=)lval) lvals
        let targetWMEOffset =
          match lval with
          | Proj _ -> 3
          | LValue.Variable _ -> 1
        lookupEnv binding {tokenIndex = index; fieldIndex = targetWMEOffset}
      | BinOp(e1,op,e2) ->
          let i1 = getInt <| loop e1
          let i2 = getInt <| loop e2
          Int <| evalOp op i1 i2
    loop e

  let mkInstanceFact instId instType : Fact = [| String "class"; Int instId; String instType|]
  let mkAssignFact instId var value : Fact = [| String "assign"; Int instId; String var; Int value|]

  type Interpreter(reteGraph : ReteGraph<LValue list * Action>) =
    let facts : Set<Fact> ref = ref Set.empty

    let getFacts () = !facts

    let getInstances() =
      Seq.choose
          (function
          | [|String "class"; Int instId; String _|] -> Some instId
          | _ -> None) <| getFacts()

    let findInstancesByType iType =
      Seq.choose
          (function
          | [|String "class"; Int instId; String iType'|] when iType = iType' -> Some instId
          | _ -> None) <| getFacts()

    let findAssignments instId =
      Seq.choose (function ([|String "assign"; Int instId'; String var; value|]) when instId' = instId -> Some(var, value) | _ -> None ) <| getFacts()

    let findInstances iType assignments =
      let facts = getFacts()
      let instFilter instId =
          Seq.forall (fun (var, value) -> Set.contains [|String "assign"; Int instId; String var; Int value|] facts) assignments
      Seq.filter instFilter <| findInstancesByType iType

    let getMaxInstanceId() = Seq.max << Seq.append (Seq.singleton 0) <| getInstances()

    let evalStr (var, value) = sprintf "%s = %A"  var value

    let findInstancesByTypeAndAssignments env instType assignments =
      let evaluatedAssignments = List.map (fun(var, exp) -> var, getInt <| evalExp env exp) assignments
      findInstances instType evaluatedAssignments, evaluatedAssignments

    let getInstanceFacts instId instType evaluatedAssignments =
      let fact = mkInstanceFact instId instType
      Seq.ofList <| fact :: (List.map (fun(var, value) -> mkAssignFact instId var value) evaluatedAssignments)

    let activateAction env : Action -> seq<Fact> =
      function
      | FindOrCreate(instType, assignments) ->
        let instances, evaluatedAssignments = findInstancesByTypeAndAssignments env instType assignments
        match Seq.length instances with
        | 0 ->
          let instId = getMaxInstanceId () + 1
          getInstanceFacts instId instType evaluatedAssignments
        | 1 -> Seq.empty
        | _ -> failwithf "multi match %A %s(%s)" instances instType (String.concat ", " <| Seq.map evalStr evaluatedAssignments)

    let deactivateAction env : Action -> seq<Fact> =
      function
      | FindOrCreate(instType, assignments) ->
        let instances, evaluatedAssignments = findInstancesByTypeAndAssignments env instType assignments
        match Seq.toList instances with
        | [] -> failwithf "could not find instance"
        | [instId] ->
          getInstanceFacts instId instType evaluatedAssignments
        | _ -> failwithf "multi match %A %s(%s)" instances instType (String.concat ", " <| Seq.map evalStr evaluatedAssignments)

    let rec activateEvalSet (inputFacts:Set<Fact>) =
      if Set.isEmpty inputFacts
      then ()
      else
        let conflictSet = Set.ofSeq <| Seq.collect (activate reteGraph) inputFacts
        facts := Set.unionMany[ !facts; inputFacts]
        let update ((lvals : LValue list, action), env : Environment) =
          let lvalEnv = lvals, env
          let newFacts = activateAction lvalEnv action
          facts := Set.unionMany[ !facts; Set.ofSeq <| newFacts]
          newFacts
        let newFacts = Set.ofSeq <| Seq.collect update conflictSet
        activateEvalSet newFacts

    let rec deactivateEvalSet (inputFacts:Set<Fact>) =
      if Set.isEmpty inputFacts
      then ()
      else
        let conflictSet = Set.ofSeq <| Seq.collect (deactivate reteGraph) inputFacts
        let update ((lvals : LValue list, action), env : Environment) =
          let lvalEnv = lvals, env
          let factsToRemove = Set.ofSeq <| deactivateAction lvalEnv action
          factsToRemove
        let newFactsToRemove = Set.ofSeq <| Seq.collect update conflictSet
        deactivateEvalSet newFactsToRemove
        facts := Set.difference !facts newFactsToRemove
        facts := Set.difference !facts inputFacts

    member __.GetFacts() = getFacts()
    member __.GetMaxInstId() = getMaxInstanceId()
    member __.GetInstancesOfType iType = findInstancesByType iType
    member __.GetAssignments instId = findAssignments instId

    member __.Add(fact:Fact) =
      if Set.contains fact !facts
      then failwithf "Fact already added %A" fact
      activateEvalSet <| Set.singleton fact

    member __.Remove(fact:Fact) =
      if not <| Set.contains fact !facts
      then failwith "Could not remove fact not added"
      deactivateEvalSet <| Set.singleton fact
