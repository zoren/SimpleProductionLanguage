namespace PatternMatchingTests

open NUnit.Framework

[<TestFixture>]
module ReteTests =
  open PatternMatching.PatternTree
  open Swensen.Unquote.Assertions

  let isClassWithXEquals5 =
    let xEquals5 = PTree(("numValue", [| Anything IntType; PatternValue <| String "X"; Anything DoubleType|]),
                      [Comparison(Variable(0,2), Eq, Const <| Double 5.0);
                        Comparison(Variable(0,0), Eq, Variable(1,0));
                      ],
                      [|Production "P"|])    
    PTree(("classType", [|Anything IntType ; PatternValue <| String "CLASS" |]), [], [|xEquals5|])


  open PatternMatching.ReteBuilder
  open PatternMatching.ReteInterpreter
  [<Test>]
  let rete() =
    let reteGraph = reteGraphFromPatternTrees <| Seq.singleton isClassWithXEquals5
    let fact = "classType", [|Int 1; String "CLASS"|]

    ignore <| activate reteGraph fact
    
    let assignFact = "numValue", [|Int 1; String "X"; Double 5.0|]
    let cs = activate reteGraph assignFact
    test <@ cs = Set.ofList ["P", [assignFact; fact]] @>

