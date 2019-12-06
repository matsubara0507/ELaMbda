module TaPL.Chap10.Parser exposing (parse)

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Parser exposing ((|.), (|=), Parser)
import Set
import TaPL.Chap10 exposing (Term(..), Ty(..))


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


tyParser : Parser Ty
tyParser =
    Parser.oneOf
        [ value "Bool" TyBool
        ]
        |. Parser.spaces
        |> Parser.andThen tyArrParser


tyArrParser : Ty -> Parser Ty
tyArrParser ty =
    Parser.oneOf
        [ Parser.succeed (TyArr ty)
            |. Parser.keyword "->"
            |. Parser.spaces
            |= Parser.lazy (\_ -> tyParser)
        , Parser.succeed ty
        ]


termParser : Context -> Parser Term
termParser ctx =
    Parser.oneOf
        [ parParser ctx
        , absParser ctx
        , valParser
        , ifParser ctx
        , varParser ctx
        ]
        |> Parser.andThen (appParser ctx)


absParser : Context -> Parser Term
absParser ctx =
    Parser.succeed Tuple.pair
        |. Parser.symbol "\\"
        |. Parser.spaces
        |= Parser.lazy (\_ -> varStrParser)
        |. Parser.spaces
        |. Parser.symbol ":"
        |. Parser.spaces
        |= Parser.lazy (\_ -> tyParser)
        |. Parser.spaces
        |. Parser.symbol "."
        |. Parser.spaces
        |> Parser.andThen (absParserN ctx)


absParserN : Context -> ( String, Ty ) -> Parser Term
absParserN ctx ( v, ty ) =
    Parser.succeed (TmAbs v ty)
        |= Parser.lazy (\_ -> termParser <| pushVar v <| incrCtx ctx)


appParser : Context -> Term -> Parser Term
appParser ctx t =
    Parser.oneOf
        [ Parser.succeed (TmApp t)
            |. Parser.backtrackable (Parser.symbol " " |. Parser.spaces)
            |= Parser.lazy (\_ -> termWithoutAppParser ctx)
            |> Parser.andThen Parser.commit
            |> Parser.andThen (appParser ctx)
        , Parser.succeed t
        ]


termWithoutAppParser : Context -> Parser Term
termWithoutAppParser ctx =
    Parser.oneOf
        [ parParser ctx
        , absParser ctx
        , valParser
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
        , reserved = Set.fromList keywords
        }


ifParser : Context -> Parser Term
ifParser ctx =
    Parser.succeed TmIf
        |. Parser.keyword "if"
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser ctx)
        |. Parser.spaces
        |. Parser.keyword "then"
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser ctx)
        |. Parser.spaces
        |. Parser.keyword "else"
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser ctx)


valParser : Parser Term
valParser =
    Parser.oneOf
        [ value "true" TmTrue
        , value "false" TmFalse
        ]


parParser : Context -> Parser Term
parParser ctx =
    Parser.succeed identity
        |. Parser.symbol "("
        |. Parser.spaces
        |= Parser.lazy (\_ -> termParser ctx)
        |. Parser.spaces
        |. Parser.symbol ")"


value : String -> a -> Parser a
value kw t =
    Parser.map (always t) (Parser.keyword kw)


keywords : List String
keywords =
    [ "true"
    , "false"
    , "if"
    , "then"
    , "else"
    ]
