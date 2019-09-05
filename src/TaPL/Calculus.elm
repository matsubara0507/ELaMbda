module TaPL.Calculus exposing (Calculus, appendLog, display, eval1, initLog, parse)

import Basics.Extra exposing (flip)
import Parser


type alias Calculus ctx t =
    { eval1 : ctx -> t -> Maybe t
    , parse : String -> Result (List Parser.DeadEnd) t
    , display : t -> String
    , init : ctx
    , logs : List t
    , syntax : String
    }


eval1 : Calculus ctx t -> Maybe (Calculus ctx t)
eval1 calc =
    List.head calc.logs
        |> Maybe.andThen (calc.eval1 calc.init)
        |> Maybe.map (flip appendLog calc)


parse : String -> Calculus ctx t -> Result (List Parser.DeadEnd) (Calculus ctx t)
parse s calc =
    calc.parse s
        |> Result.map (flip appendLog (initLog calc))


appendLog : t -> Calculus ctx t -> Calculus ctx t
appendLog t calc =
    { calc | logs = t :: calc.logs }


initLog : Calculus ctx t -> Calculus ctx t
initLog calc =
    { calc | logs = [] }


display : Calculus ctx t -> List String
display calc =
    List.map calc.display calc.logs
