module Lambda.Parser exposing (ifParser, isZeroParser, parParser, parse, parser, predParser, succParser, termParser, valParser, value)

import Lambda exposing (Term(..))
import Parser exposing ((|.), (|=), Parser)


parse : String -> Result (List Parser.DeadEnd) Lambda.Term
parse =
    Parser.run parser


parser : Parser Lambda.Term
parser =
    termParser |. Parser.end


termParser : Parser Lambda.Term
termParser =
    Parser.oneOf
        [ valParser
        , ifParser
        , succParser
        , predParser
        , isZeroParser
        , parParser
        ]


ifParser : Parser Lambda.Term
ifParser =
    Parser.succeed TmIf
        |. Parser.keyword "if"
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser)
        |. Parser.spaces
        |. Parser.keyword "then"
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser)
        |. Parser.spaces
        |. Parser.keyword "else"
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser)


succParser : Parser Lambda.Term
succParser =
    Parser.succeed TmSucc
        |. Parser.keyword "succ"
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser)


predParser : Parser Lambda.Term
predParser =
    Parser.succeed TmPred
        |. Parser.keyword "pred"
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser)


isZeroParser : Parser Lambda.Term
isZeroParser =
    Parser.succeed TmIsZero
        |. Parser.keyword "iszero"
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser)


parParser : Parser Lambda.Term
parParser =
    Parser.succeed identity
        |. Parser.symbol "("
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser)
        |. Parser.spaces
        |. Parser.symbol ")"


valParser : Parser Lambda.Term
valParser =
    Parser.oneOf
        [ value "true" TmTrue
        , value "false" TmFalse
        , Parser.int |> Parser.map Lambda.fromInt
        ]


value : String -> Lambda.Term -> Parser Lambda.Term
value kw t =
    Parser.map (always t) (Parser.keyword kw)
