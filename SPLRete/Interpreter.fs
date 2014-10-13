namespace SPLRete

module Interpreter =
  open PatternMatching.ReteNetwork
  open PatternMatching.ReteInterpreter
  open PatternMatching.PatternTree
  open SimpleProductionLanguage.AST

//  let findInstancesByType iType =
//      Seq.choose 
//          (function
//          | Instance(instId, iType') when iType = iType' -> Some instId
//          | _ -> None)
//
//  let evalAction facts (binding) (action:Action) : Fact seq =
//      match action with
//      | FindOrCreate(instType, assignments) ->
//          let evaluatedAssignments = List.map (fun(var, exp) -> var, evalExp binding exp) assignments
//          let instances = findInstances instType evaluatedAssignments facts
//          match Seq.length instances with
//          | 0 -> 
//              let newInstId = getMaxInstanceId facts + 1 
//              Seq.append 
//                  (Seq.singleton <| Instance(newInstId, instType)) 
//                  (Seq.map (fun (var, value) -> Assignment(newInstId,var, value)) evaluatedAssignments)
//          | 1 -> Seq.empty
//          | _ -> failwith "multi match"
//  
  let evalOp =
      function | Plus -> (+) | Minus -> (-) | Times -> (*) | Division -> (/)  
  
  let evalExp ((lvals : LValue list, binding: WME list) as env) e =
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
          let (Int i1) = loop e1
          let (Int i2) = loop e2
          Int <| evalOp op i1 i2  
    loop e

  type Interpreter(reteGraph : ReteGraph<LValue list * Action>) =
    let facts = ref Set.empty

    let getFacts () = !facts

    let findInstancesByType iType =
      Seq.choose 
          (function
          | "class",[|Int instId; String iType'|] when iType = iType' -> Some instId
          | _ -> None) <| getFacts()
    
    let findInstances iType assignments =
      let facts = getFacts()
      let instFilter instId =
          Seq.forall (fun (var, value) -> Set.contains ("assign",[|Int instId; String var; value|]) facts) assignments
      Seq.filter instFilter <| findInstancesByType iType

    let getMaxInstanceId() = 
      let getInstId =
        function 
        | "class", [|Int instId; String _|] -> instId
        | "assign", [|Int instId; String _; _|] -> instId                
      let instIds = Seq.map getInstId <| getFacts()
      Seq.max <| Seq.append (Seq.singleton 0) instIds
  
    let evalAction env : Action -> seq<Fact> =
      function
      | FindOrCreate(instType, assignments) ->
        let evaluatedAssignments = List.map (fun(var, exp) -> var, evalExp env exp) assignments
        let instances = findInstances instType evaluatedAssignments
        match Seq.length instances with
        | 0 -> 
          let instId = Int <| getMaxInstanceId () + 1 
          let fact : Fact = "class", [|instId; String instType|]
          Seq.ofList <| fact :: (List.map (fun(var, value) -> "assign", [|instId;String var; value|]) evaluatedAssignments)
        | 1 -> Seq.empty
        | _ -> failwith "multi match"
      
    let evalConflict ((lvals : LValue list, action), wmes : WME list) =
      let env = lvals, wmes
      evalAction env action

    let rec activateEval (fact:Fact) =
      if Set.contains fact !facts
      then ()
      let conflictSet = activate reteGraph fact
      let newFacts = Set.ofSeq <| Seq.collect evalConflict conflictSet
      facts := Set.union !facts newFacts
      Seq.iter activateEval newFacts

//    let activateEval (fact:Fact) =
//      let rec loop fact =
//        if not << Set.contains fact <| getFacts()
//        then
//          let conflictSet = activate reteGraph fact
//          let evalConflictSet ((lvals : LValue list, action), wmes : WME list) =
//            let env = lvals, wmes
//            let oldFacts = getFacts()
//            let actionFacts = evalAction env action
//            let newFacts = Set.union (getFacts()) (Set.ofSeq actionFacts)
//            if oldFacts <> newFacts
//            then
//              facts := newFacts
//             // let factsNotInSet = Seq.filter (fun f -> not <| Set.contains f oldFacts) actionFacts
//              Seq.iter loop actionFacts
//          Set.iter evalConflictSet conflictSet
//      loop fact

    

    member __.GetFacts() = getFacts()

    member __.GetInstancesOfType iType = findInstancesByType iType

    member __.Add(fact:Fact) =
      if Set.contains fact !facts
      then failwith "Fact already added"            
      activateEval fact

//    member __.Remove(fact:Fact) =
//      if not <| Set.contains fact !userFacts
//      then failwith "Could not remove fact not added"            
//      failwith ""
