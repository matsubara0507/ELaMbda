module TaPL exposing (Chapter(..), chapters, display, eval1, init, parse, syntax, toString, typecheck)

import Parser
import TaPL.Calculus as Calculus exposing (Calculus)
import TaPL.Chap10 as Chap10
import TaPL.Chap10.Parser as Chap10
import TaPL.Chap4 as Chap4
import TaPL.Chap4.Parser as Chap4
import TaPL.Chap7 as Chap7
import TaPL.Chap7.Parser as Chap7


type Chapter
    = Chap0
    | Chap4 (Calculus () Chap4.Term Never)
    | Chap7 (Calculus Chap7.Context Chap7.Term Never)
    | Chap10 (Calculus Chap10.Context Chap10.Term Chap10.Ty)


init : String -> Chapter
init s =
    case s of
        "chap4" ->
            Chap4
                { eval1 = \_ -> Chap4.eval1
                , display = Chap4.display
                , parse = Chap4.parse
                , init = ()
                , logs = []
                , syntax = Chap4.syntax
                , typeof = Nothing
                }

        "chap7" ->
            Chap7
                { eval1 = Chap7.eval1
                , display = Chap7.display
                , parse = Chap7.parse
                , init = []
                , logs = []
                , syntax = Chap7.syntax
                , typeof = Nothing
                }

        "chap10" ->
            Chap10
                { eval1 = Chap10.eval1
                , display = Chap10.display
                , parse = Chap10.parse
                , init = []
                , logs = []
                , syntax = Chap10.syntax
                , typeof = Just Chap10.typeof
                }

        _ ->
            Chap0


toString : Chapter -> String
toString chap =
    case chap of
        Chap4 _ ->
            "chap4"

        Chap7 _ ->
            "chap7"

        Chap10 _ ->
            "chap10"

        Chap0 ->
            "chap0"


chapters : List String
chapters =
    [ "chap4", "chap7", "chap10" ]


eval1 : Chapter -> Maybe Chapter
eval1 chap =
    case chap of
        Chap0 ->
            Nothing

        Chap4 calc ->
            Maybe.map Chap4 (Calculus.eval1 calc)

        Chap7 calc ->
            Maybe.map Chap7 (Calculus.eval1 calc)

        Chap10 calc ->
            Maybe.map Chap10 (Calculus.eval1 calc)


display : Chapter -> List String
display chap =
    case chap of
        Chap0 ->
            []

        Chap4 calc ->
            Calculus.display calc

        Chap7 calc ->
            Calculus.display calc

        Chap10 calc ->
            Calculus.display calc


parse : Chapter -> String -> Result (List Parser.DeadEnd) Chapter
parse chap str =
    case chap of
        Chap0 ->
            Err []

        Chap4 calc ->
            Result.map Chap4 (Calculus.parse str calc)

        Chap7 calc ->
            Result.map Chap7 (Calculus.parse str calc)

        Chap10 calc ->
            Result.map Chap10 (Calculus.parse str calc)


syntax : Chapter -> String
syntax chap =
    case chap of
        Chap0 ->
            ""

        Chap4 calc ->
            calc.syntax

        Chap7 calc ->
            calc.syntax

        Chap10 calc ->
            calc.syntax


typecheck : Chapter -> Bool
typecheck chap =
    case chap of
        Chap0 ->
            True

        Chap4 calc ->
            Calculus.typecheck calc

        Chap7 calc ->
            Calculus.typecheck calc

        Chap10 calc ->
            Calculus.typecheck calc
