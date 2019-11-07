module TaPL.Chap7.Parser exposing (parse)

import Basics.Extra exposing (flip)
import Debug
import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Set
import TaPL.Chap7 exposing (Term(..))


type alias Context =
    { env : Dict String Int
    , depth : Int
    }


incrCtx : Context -> Context
incrCtx ctx =
    { ctx | depth = ctx.depth + 1, env = Dict.map (\_ v -> v + 1) ctx.env }


pushVar : String -> Context -> Context
pushVar v ctx =
    { ctx | env = Dict.insert v 0 ctx.env }


iniCtx : Context
iniCtx =
    { env = Dict.empty, depth = 0 }


parse : String -> Result (List Parser.DeadEnd) Term
parse =
    Parser.run parser


parser : Parser Term
parser =
    termParser iniCtx |. Parser.end


termParser : Context -> Parser Term
termParser ctx =
    Parser.oneOf
        [ parParser ctx
        , absParser ctx
        , varParser ctx
        ]
        |> Parser.andThen (appParser ctx)


absParser : Context -> Parser Term
absParser ctx =
    Parser.succeed identity
        |. Parser.symbol "\\"
        |. Parser.spaces
        |= varStrParser
        |. Parser.spaces
        |. Parser.symbol "."
        |. Parser.spaces
        |> Parser.andThen (absParserN ctx)


absParserN : Context -> String -> Parser Term
absParserN ctx v =
    Parser.succeed (TmAbs v)
        |= Parser.lazy (\_ -> termParser <| pushVar v <| incrCtx ctx)


appParser : Context -> Term -> Parser Term
appParser ctx t =
    Parser.oneOf
        [ Parser.succeed (TmApp t)
            |. Parser.backtrackable (Parser.symbol " " |. Parser.spaces)
            |= Parser.lazy (\_ -> Parser.backtrackable (termWithoutAppParser ctx))
            |> Parser.andThen Parser.commit
            |> Parser.andThen (appParser ctx)
        , Parser.succeed t
        ]


termWithoutAppParser : Context -> Parser Term
termWithoutAppParser ctx =
    Parser.oneOf
        [ parParser ctx
        , absParser ctx
        , varParser ctx
        ]


varParser : Context -> Parser Term
varParser ctx =
    varStrParser
        |> Parser.andThen (lookupVar ctx)
        |> Parser.map (flip TmVar ctx.depth)


lookupVar : Context -> String -> Parser Int
lookupVar ctx s =
    Dict.get s ctx.env
        |> Maybe.map Parser.succeed
        |> Maybe.withDefault (Parser.problem ("undefined variable: " ++ s))


varStrParser : Parser String
varStrParser =
    Parser.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_' || c == '\''
        , reserved = Set.fromList []
        }


parParser : Context -> Parser Term
parParser ctx =
    Parser.succeed identity
        |. Parser.symbol "("
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser ctx)
        |. Parser.spaces
        |. Parser.symbol ")"
