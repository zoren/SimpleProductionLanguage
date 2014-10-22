namespace SimpleProductionLanguage

open FParsec

module Parser =
  let ws : Parser<unit, unit> = ( spaces >>. many( pstring "//" >>. skipRestOfLine true >>. spaces)) >>% ()

  let isAsciiIdStart c = isAsciiLetter c || c = '_'

  let isAsciiIdContinue c = isAsciiLetter c || isDigit c || c = '_' || c = '\''

  let id = identifier (IdentifierOptions(isAsciiIdStart = isAsciiIdStart,
                              isAsciiIdContinue = isAsciiIdContinue)) .>> ws

  let pint_ws = pint32 .>> ws

  let str_ws s = pstring s .>> ws
  let charws c : Parser<char, unit> = pchar c .>> ws

  let qmark = charws '?'
  let hash = charws '#'
  let lpar = charws '('
  let rpar = charws ')'
  let dot = charws '.'

  let comma = charws ','

  let semi = charws ';'
  let colon = charws ':'
  let eq = charws '='

  let lcurly = charws '{'
  let rcurly = charws '}'

  let parens p = between lpar rpar p

  open SimpleProductionLanguage.AST

  let plvalue =
    sepBy1 id dot |>> fun (id::ids) -> Seq.fold (fun acc id -> Proj(acc,id)) (id |> Variable) ids

  let expOpp = OperatorPrecedenceParser<Expression,unit,unit>()
  let pexp = expOpp.ExpressionParser

  let ExpTermParser =
    choice [
      pint_ws |>> Constant
      plvalue |>> Deref
      parens pexp
    ]

  expOpp.TermParser <- ExpTermParser

  let mkBin op l r = BinOp(l, op, r)

  expOpp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, mkBin Plus))
  expOpp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, mkBin Minus))
  expOpp.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, mkBin Times))
  expOpp.AddOperator(InfixOperator("/", ws, 2, Associativity.Left, mkBin Division))

  let twoExps = parens (pexp .>> (charws ',') .>>. pexp)

  let pcond =
    choice [
        str_ws "true" >>% True
        str_ws "part_of" >>. twoExps |>> PartOf
        pexp .>> (charws '<') .>>. pexp |>> LessThan
    ]

  let passignment = id .>> (str_ws ":=") .>>. pexp

  let action =
    choice [
        (str_ws "find_or_create") >>. id .>>. (parens (sepBy passignment comma)) |>> FindOrCreate
    ]

  let pabstractions = sepBy1 (id .>> colon .>>. id) comma

  let rule : Parser<Rule, unit> =
    tuple3
        (charws '\\' >>. pabstractions)
        (str_ws "->" >>. pcond .>> qmark)
        action

  let rules =
    ws >>. many rule
