namespace SimpleProductionLanguage

open SimpleProductionLanguage.AST

module Interpreter =
    type InstanceId = int

    type RTValue = 
        | Int of int
        | InstanceRef of InstanceId

    type Fact =
        | Instance of InstanceId * InstanceType
        | Assignment of InstanceId * FieldName * RTValue

    let findInstancesByType iType =
        Seq.choose 
            (function
            | Instance(instId, iType') when iType = iType' -> Some instId
            | _ -> None)

    let findAssignments instId field =
        Seq.choose 
            (function
            | Assignment(instId', field', value') when instId = instId' && field = field' -> Some value'
            | _ -> None)

    let bindAbstractions facts abstrs =
        let rec bindFactsToAbstr (abstrs:Abstractions) =
            match abstrs with
            | Abstr(var, iType) -> Seq.map (fun instId -> Map.ofList[var, instId]) <| findInstancesByType iType facts
            | Abstrs(var, iType, abstrs') ->
                let bindings = bindFactsToAbstr abstrs'
                let instBindings = findInstancesByType iType facts
                Seq.collect (fun instId -> Seq.map (fun binding -> Map.add var instId binding) bindings) instBindings                
        bindFactsToAbstr abstrs

    let rec getType var abstrs =
        match abstrs with
        | Abstr(var', iType') when var = var' -> iType'
        | Abstrs(var', iType', abstrs) ->
            if var = var'
            then iType'
            else getType var abstrs
        | _ -> failwithf "Cannot find type of var: '%s'" var

    let bindRule facts (rule : Rule) : Map<LValue, RTValue> seq =
        let abstrs, _, _ = rule
        let rec bindLValValue accMap lval =
            match lval with
            | Variable var ->
                let iType = getType var abstrs
                Seq.map (fun instId -> Map.add lval (InstanceRef instId) accMap) <| findInstancesByType iType facts
            | Proj(lval', fieldName) ->
                let maps = bindLValValue accMap lval'
                let f binding =
                    let (InstanceRef instId) = Map.find lval' binding
                    let assignedValues = findAssignments instId fieldName facts
                    Seq.map (fun value -> Map.add lval value binding) assignedValues
                Seq.collect f maps
        let lvalDom = lvalDomRule rule
        let rec binder accMap lvals =               
            match lvals with
            | [] -> Seq.singleton accMap
            | lval::lvals ->
                let bindings = bindLValValue accMap lval
                Seq.collect (fun binding -> binder binding lvals) bindings
        let rec lvalsInAbstr abstr =
            match abstr with
            | Abstr(var, _) -> Set.singleton <| Variable var
            | Abstrs(var, _, abstrs) -> Set.union (Set.singleton <| Variable var) (lvalsInAbstr abstrs)
        let lvalAndAbstr = Set.toList <| Set.union lvalDom (lvalsInAbstr abstrs)
        binder Map.empty <| lvalAndAbstr

    let evalOp =
        function | Plus -> (+) | Minus -> (-) | Times -> (*) | Division -> (/)

    let rec evalExp binding =
        function
        | Constant i -> Int i
        | Deref lval -> Map.find lval binding
        | BinOp(e1,op,e2) -> 
            let (Int i1) = evalExp binding e1
            let (Int i2) = evalExp binding e2
            Int <| evalOp op i1 i2

    let evalCond (binding:Map<LValue,RTValue>) (cond:Condition) =
        match cond with
        | True -> true
        | LessThan(e1, e2) ->
            let (Int i1) = evalExp binding e1
            let (Int i2) = evalExp binding e2        
            i1 < i2

    let findInstances iType assignments facts =
        let instFilter instId =
            Seq.forall (fun (var, value) -> Set.contains (Assignment(instId, var, value)) facts) assignments
        Seq.filter instFilter <| findInstancesByType iType facts

    let getMaxInstanceId facts = 
        let getInstId =
            function 
            | Instance(instId, _) -> instId
            | Assignment(instId, _, _) -> instId                
        let instIds = Seq.map getInstId facts
        Seq.max <| Seq.append (Seq.singleton 0) instIds

    let evalAction facts (binding) (action:Action) : Fact seq =
        match action with
        | FindOrCreate(instType, assignments) ->
            let evaluatedAssignments = List.map (fun(var, exp) -> var, evalExp binding exp) assignments
            let instances = findInstances instType evaluatedAssignments facts
            match Seq.length instances with
            | 0 -> 
                let newInstId = getMaxInstanceId facts + 1 
                Seq.append 
                    (Seq.singleton <| Instance(newInstId, instType)) 
                    (Seq.map (fun (var, value) -> Assignment(newInstId,var, value)) evaluatedAssignments)
            | 1 -> Seq.empty
            | _ -> failwith "multi match"

    let evalRule facts ((_,cond,action) as rule:Rule) =
        let bindings = bindRule facts rule
        let evalBinding facts binding =
            if evalCond binding cond
            then Set.union
                    (Set.ofSeq <| evalAction facts binding action)
                    facts
            else facts
        Seq.fold evalBinding facts bindings

    let evalRules facts rules =
        let rec loop facts =
            let newFacts = Seq.fold evalRule facts rules        
            if newFacts = facts
            then newFacts
            else loop newFacts
        loop facts

    type Interpreter(rules : Rule seq) =
        let facts = ref Set.empty
        let userFacts = ref Set.empty

        let getFacts () = Set.union !facts !userFacts

        let rec leFix oldFacts =
            let addedFacts = evalRules oldFacts rules
            let newFacts = Set.union oldFacts addedFacts
            if oldFacts = newFacts
            then
                newFacts 
            else
                leFix newFacts

        member x.GetFacts() = !facts

        member x.GetInstancesOfType iType = findInstancesByType iType !facts

        member x.GetFreshInstanceId() = getMaxInstanceId !facts + 1 
        
        member x.Add(fact:Fact) =
            if Set.contains fact !userFacts
            then failwith "Fact already added"            
            let newUserFacts = Set.add fact !userFacts
            let newFacts = leFix newUserFacts
            let oldFacts = getFacts()
            facts := newFacts
            userFacts := newUserFacts
            Set.difference newFacts oldFacts

        member x.Remove(fact:Fact) =
            if not <| Set.contains fact !userFacts
            then failwith "Could not remove fact not added"            
            let oldFacts = !facts
            let newUserFacts = Set.remove fact !userFacts
            let newFacts = leFix newUserFacts
            facts := newFacts
            userFacts := newUserFacts
            Set.difference oldFacts newFacts
        
        member x.Modify(oldFact:Fact, newFact:Fact) =
            if not <| Set.contains oldFact !userFacts
            then failwith "Modify: Could not remove fact not added"
            if Set.contains newFact !userFacts
            then failwith "Modify: Fact already added"            
            let oldFacts = !facts
            let newUserFacts = Set.remove oldFact !userFacts
            let newFacts = leFix newUserFacts
            let newUserFacts' = Set.add newFact newUserFacts
            let newFacts' = leFix newUserFacts'
            facts := newFacts'
            userFacts := newUserFacts'
            let addedFacts = Set.difference newFacts' oldFacts
            let removedFacts = Set.difference oldFacts (Set.union newFacts newFacts')
            addedFacts, removedFacts
