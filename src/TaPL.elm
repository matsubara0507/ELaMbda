module TaPL exposing (Chapter(..), Model, chapterFromString, chapterToString, display, eval1, init, parse, syntax)

import Parser
import TaPL.Calculus as Calculus exposing (Calculus)
import TaPL.Chap4 as Chap4
import TaPL.Chap4.Parser as Chap4
import TaPL.Chap7 as Chap7
import TaPL.Chap7.Parser as Chap7


type Chapter
    = Chap0
    | Chap4
    | Chap7


chapterFromString : String -> Chapter
chapterFromString s =
    case s of
        "chap4" ->
            Chap4

        "chap7" ->
            Chap7

        _ ->
            Chap0


chapterToString : Chapter -> String
chapterToString chap =
    case chap of
        Chap4 ->
            "chap4"

        Chap7 ->
            "chap7"

        Chap0 ->
            "chap0"


type alias Model =
    { chap4 : Calculus () Chap4.Term
    , chap7 : Calculus Chap7.Context Chap7.Term
    }


init : Model
init =
    { chap4 =
        { eval1 = \_ -> Chap4.eval1
        , display = Chap4.display
        , parse = Chap4.parse
        , init = ()
        , logs = []
        , syntax = Chap4.syntax
        }
    , chap7 =
        { eval1 = Chap7.eval1
        , display = Chap7.display
        , parse = Chap7.parse
        , init = []
        , logs = []
        , syntax = Chap7.syntax
        }
    }


eval1 : Chapter -> Model -> Maybe Model
eval1 chap model =
    case chap of
        Chap0 ->
            Nothing

        Chap4 ->
            Maybe.map (\calc -> { model | chap4 = calc }) (Calculus.eval1 model.chap4)

        Chap7 ->
            Maybe.map (\calc -> { model | chap7 = calc }) (Calculus.eval1 model.chap7)


display : Chapter -> Model -> List String
display chap model =
    case chap of
        Chap0 ->
            []

        Chap4 ->
            Calculus.display model.chap4

        Chap7 ->
            Calculus.display model.chap7


parse : Chapter -> String -> Model -> Result (List Parser.DeadEnd) Model
parse chap str model =
    case chap of
        Chap0 ->
            Err []

        Chap4 ->
            Result.map (\calc -> { model | chap4 = calc }) (Calculus.parse str model.chap4)

        Chap7 ->
            Result.map (\calc -> { model | chap7 = calc }) (Calculus.parse str model.chap7)


syntax : Chapter -> Model -> String
syntax chap model =
    case chap of
        Chap0 ->
            ""

        Chap4 ->
            model.chap4.syntax

        Chap7 ->
            model.chap7.syntax
