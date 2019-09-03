module TaPL.Chap4.Parser exposing (ifParser, isZeroParser, parParser, parse, parser, predParser, succParser, termParser, valParser, value)

import Parser exposing ((|.), (|=), Parser)
import TaPL.Chap4 as Chap4 exposing (Term(..))


parse : String -> Result (List Parser.DeadEnd) Term
parse =
    Parser.run parser


parser : Parser Term
parser =
    termParser |. Parser.end


termParser : Parser Term
termParser =
    Parser.oneOf
        [ valParser
        , ifParser
        , succParser
        , predParser
        , isZeroParser
        , parParser
        ]


ifParser : Parser Term
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


succParser : Parser Term
succParser =
    Parser.succeed TmSucc
        |. Parser.keyword "succ"
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser)


predParser : Parser Term
predParser =
    Parser.succeed TmPred
        |. Parser.keyword "pred"
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser)


isZeroParser : Parser Term
isZeroParser =
    Parser.succeed TmIsZero
        |. Parser.keyword "iszero"
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser)


parParser : Parser Term
parParser =
    Parser.succeed identity
        |. Parser.symbol "("
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser)
        |. Parser.spaces
        |. Parser.symbol ")"


valParser : Parser Term
valParser =
    Parser.oneOf
        [ value "true" TmTrue
        , value "false" TmFalse
        , Parser.int |> Parser.map Chap4.fromInt
        ]


value : String -> Term -> Parser Term
value kw t =
    Parser.map (always t) (Parser.keyword kw)
