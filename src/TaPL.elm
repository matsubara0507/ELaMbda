module TaPL exposing (Calculus, Chapter(..), Model, chapterFromString, chapterToString, display, eval1, init, parse)

import Parser
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


type alias Calculus ctx t =
    { eval1 : ctx -> t -> Maybe t
    , display : t -> String
    , parse : String -> Result (List Parser.DeadEnd) t
    , init : ctx
    , logs : List t
    }


appendLog : t -> Calculus ctx t -> Calculus ctx t
appendLog t calc =
    { calc | logs = t :: calc.logs }


initLog : Calculus ctx t -> Calculus ctx t
initLog calc =
    { calc | logs = [] }


displayLog : Calculus ctx t -> List String
displayLog calc =
    List.map calc.display calc.logs


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
        }
    , chap7 =
        { eval1 = Chap7.eval1
        , display = Chap7.display
        , parse = Chap7.parse
        , init = []
        , logs = []
        }
    }


eval1 : Chapter -> Model -> Maybe Model
eval1 chap model =
    case chap of
        Chap0 ->
            Nothing

        Chap4 ->
            List.head model.chap4.logs
                |> Maybe.andThen (model.chap4.eval1 model.chap4.init)
                |> Maybe.map (\t -> { model | chap4 = appendLog t model.chap4 })

        Chap7 ->
            List.head model.chap7.logs
                |> Maybe.andThen (model.chap7.eval1 model.chap7.init)
                |> Maybe.map (\t -> { model | chap7 = appendLog t model.chap7 })


display : Chapter -> Model -> List String
display chap model =
    case chap of
        Chap0 ->
            []

        Chap4 ->
            displayLog model.chap4

        Chap7 ->
            displayLog model.chap7


parse : Chapter -> String -> Model -> Result (List Parser.DeadEnd) Model
parse chap str model =
    case chap of
        Chap0 ->
            Err []

        Chap4 ->
            model.chap4.parse str
                |> Result.map (\t -> { model | chap4 = appendLog t <| initLog model.chap4 })

        Chap7 ->
            model.chap7.parse str
                |> Result.map (\t -> { model | chap7 = appendLog t <| initLog model.chap7 })
