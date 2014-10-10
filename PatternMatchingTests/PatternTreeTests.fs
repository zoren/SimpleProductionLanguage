namespace PatternMatchingTests

open NUnit.Framework

[<TestFixture>]
module PatternTreeTests =
  open PatternMatching.PatternTree
  open Swensen.Unquote.Assertions

  [<Test>]
  let ``lala``() =
    let ptree = PTree(("classType", [|Anything; PatternValue <| String "CLASS" |]), [], [|Production "P"|])
    let state = mkEmptyState ptree
    let fact = "classType", [|Int 1; String "CLASS"|]

    test <@ activate state fact = Set.ofList ["P", [fact]] @>
  // tree to filter instances of type CLASS with X = 5.0
  let isClassWithXEquals5 =
    let xEquals5 = PTree(("value", [| Anything; PatternValue <| String "X"; Anything |]),
                      [Comparison(Variable(0,2), Eq, Const <| Double 5.0);
                        Comparison(Variable(0,0), Eq, Variable(1,0));
                      ],
                      [|Production "P"|])    
    PTree(("classType", [|Anything; PatternValue <| String "CLASS" |]), [], [|xEquals5|])

  [<Test>]
  let ``test activation``() =
    let state = mkEmptyState isClassWithXEquals5

    let fact = "classType", [|Int 1; String "CLASS"|]

    let cs = activate state fact

    test <@ Set.isEmpty cs @>

  [<Test>]
  let ``test tests``() =
    let state = mkEmptyState isClassWithXEquals5

    let fact = "classType", [|Int 1; String "CLASS"|]

    ignore <| activate state fact
    
    let assignFact = "value", [|Int 1; String "X"; Double 5.0|]
    
    test <@ activate state assignFact = Set.ofList ["P", [assignFact; fact]] @>

  [<Test>]
  let ``test deactivation``() =
    let state = mkEmptyState isClassWithXEquals5

    let fact = "classType", [|Int 1; String "CLASS"|]

    ignore <| activate state fact
    
    let assignFact = "value", [|Int 1; String "X"; Double 5.0|]
    
    ignore <| activate state assignFact

    test <@ deactivate state assignFact = Set.ofList ["P", [assignFact; fact]] @>

  [<Test>]
  let ``test no match``() =
    let state = mkEmptyState isClassWithXEquals5

    let fact = "classType", [|Int 1; String "CLASS"|]

    ignore <| activate state fact
    
    let assignFact = "value", [|Int 1; String "X"; Double 6.0|]
    
    test <@ Set.isEmpty <| activate state assignFact @>
