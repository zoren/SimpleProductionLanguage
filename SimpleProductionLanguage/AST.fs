namespace SimpleProductionLanguage

module AST =
    type InstanceType = string
    type VariableName = string
    type FieldName = string

    type Abstractions =
        | Abstr of VariableName * InstanceType
        | Abstrs of VariableName * InstanceType * Abstractions
    
    type BinOperator = Plus | Minus | Times | Division

    type LValue =
        | Variable of VariableName
        | Proj of LValue * FieldName

    type Expression =
        | Constant of int
        | Deref of LValue
        | BinOp of Expression * BinOperator * Expression
        
    type Condition = 
        | True
        | LessThan of Expression * Expression

    type Action =
    // assign
        | FindOrCreate of InstanceType * (VariableName * Expression) list
        | Assign of VariableName * VariableName * Expression

    type Rule = Abstractions * Condition * Action

    let rec abstrToSeq =
      function
      | Abstr(varName, instType) -> Seq.singleton (varName, instType)
      | Abstrs(varName, instType, abstrs) -> Seq.append (Seq.singleton (varName, instType)) <| abstrToSeq abstrs

    let rec lvalDomExp =
        function
        | Constant _ -> Set.empty
        | Deref lval -> Set.singleton lval
        | BinOp(e1, _, e2) -> Set.union (lvalDomExp e1) (lvalDomExp e2)

    let lvalDomCond =
        function
        | True -> Set.empty
        | LessThan(e1,e2) -> Set.union (lvalDomExp e1) (lvalDomExp e2)

    let lvalDomAction =
        function
        | FindOrCreate(_, assignments) -> Set.unionMany <| Seq.map (fun(_, e) -> lvalDomExp e) assignments

    let lvalDomRule ((_,cond,action):Rule) =
        Set.union (lvalDomCond cond) (lvalDomAction action)

    let rec lvalInsideOut (lval:LValue) : VariableName * FieldName seq =
        match lval with
        | Variable var -> var, Seq.empty
        | Proj(lval', fieldName) ->
            let var, fields = lvalInsideOut lval'
            var, Seq.append fields (Seq.singleton fieldName)