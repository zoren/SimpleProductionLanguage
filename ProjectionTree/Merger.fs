namespace ProjectionTree

module Merger =
  open ProjectionTree.Tree

  let shallowSort<'Action when 'Action : comparison> (trees:Node<'Action> list) : Node<'Action> list  =
    List.sort trees
  let patternsAreCongruent p1 p2 = p1 = p2
  let rec mergeList =
    function
    | ptree1::((ptree2::ptrees) as tail) ->
      match merge2 ptree1 ptree2 with
        | None -> ptree1 :: mergeList tail
        | Some newPtree -> mergeList ( newPtree :: ptrees )
    | t -> t
  and merge2 (t1:Node<'Action>) (t2:Node<'Action>) : Node<'Action> option =
    match t1, t2 with
    | PatternNode(pnode1, children1), PatternNode(pnode2, children2) when patternsAreCongruent pnode1 pnode2 ->
      Some(PatternNode(pnode1, List.append children1 children2))
    | _ -> None
  