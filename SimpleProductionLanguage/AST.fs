namespace SimpleProductionLanguage

module AST =
    type InstanceType = string
    type VariableName = string
    type FieldName = string

    type Abstractions = (VariableName * InstanceType) list

    type BinOperator = Plus | Minus | Times | Division

    type LValue =
        | Variable of VariableName
        | Proj of LValue * FieldName

    type Expression =
        | Constant of int
        | Deref of LValue
        | BinOp of Expression * BinOperator * Expression

    type ComparisonOperator = EQ | LT

    type Condition =
        | True
        | Comparison of Expression * ComparisonOperator * Expression
        | PartOf of Expression * Expression

    type Action =
        | FindOrCreate of InstanceType * (VariableName * Expression) list

    type Rule = Abstractions * Condition * Action

    type LValueHistogram = Map<LValue, int>

    let addToMap map (lval:LValue) n =
      match Map.tryFind lval map with
      | None -> Map.add lval n map
      | Some n' -> Map.add lval (n + n') map

    let histogramUnion mapl mapr = Map.fold addToMap mapl mapr
    let histogramUnionMany maps = Seq.reduce histogramUnion maps

    let singletonHistogram lval = Map.add lval 1 Map.empty

    let rec lvalDomExp =
        function
        | Constant _ -> Map.empty
        | Deref lval -> singletonHistogram lval
        | BinOp(e1, _, e2) -> histogramUnion (lvalDomExp e1) (lvalDomExp e2)

    let lvalDomCond =
        function
        | True -> Map.empty
        | Comparison(e1, _, e2) -> histogramUnion (lvalDomExp e1) (lvalDomExp e2)
        | PartOf(e1,e2) -> histogramUnion (lvalDomExp e1) (lvalDomExp e2)

    let lvalDomAction =
        function
        | FindOrCreate(_, assignments) -> histogramUnionMany <| Seq.map (fun(_, e) -> lvalDomExp e) assignments

    let rec lvalsInAbstr (abstrs:Abstractions) =
      histogramUnionMany <| Seq.map (fun (var, _) -> singletonHistogram <| Variable var) abstrs

    let lvalDomRule ((abstrs,cond,action):Rule) =
        histogramUnionMany [lvalsInAbstr abstrs
                            lvalDomCond cond
                            lvalDomAction action]

    let collateHistogram (histogram:LValueHistogram) : LValueHistogram =
      let addLVal hist lval n =
        let rec loop lval =
          match lval with
          | Variable _ -> addToMap hist lval n
          | Proj(lval', _) -> addToMap (loop lval') lval n
        loop lval
      Map.fold addLVal Map.empty histogram

    let rec lvalToAllProjections (lval:LValue) : seq<LValue> =
      Seq.append
        (match lval with
          | Variable _ -> Seq.empty
          | Proj(lval', _) -> lvalToAllProjections lval')
        (Seq.singleton lval)

    let histogramToOrder (histogram:LValueHistogram) =
      let lvalOrder = Seq.map fst << Seq.sortBy (fun(_,n) -> -n) << Map.toArray <| collateHistogram histogram
      let expanded = Seq.collect lvalToAllProjections lvalOrder
      Seq.distinct expanded

    let rec lvalInsideOut (lval:LValue) : VariableName * FieldName seq =
        match lval with
        | Variable var -> var, Seq.empty
        | Proj(lval', fieldName) ->
            let var, fields = lvalInsideOut lval'
            var, Seq.append fields (Seq.singleton fieldName)

    let evalOp = function | Plus -> (+) | Minus -> (-) | Times -> (*) | Division -> (/)

    let compOpToFunc =
      function
      | LT -> (<)
      | EQ -> (=)

    let getType var (abstrs:Abstractions) = snd <| List.find (fun (var', _) -> var = var') abstrs
