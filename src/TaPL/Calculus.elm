module TaPL.Calculus exposing (Calculus, appendLog, display, eval1, initLog, parse, typecheck)

import Basics.Extra exposing (flip)
import Parser


type alias Calculus ctx t ty =
    { eval1 : ctx -> t -> Maybe t
    , parse : String -> Result (List Parser.DeadEnd) t
    , display : t -> String
    , init : ctx
    , logs : List t
    , syntax : String
    , typeof : Maybe (ctx -> t -> Maybe ty)
    }


eval1 : Calculus ctx t ty -> Maybe (Calculus ctx t ty)
eval1 calc =
    List.head calc.logs
        |> Maybe.andThen (calc.eval1 calc.init)
        |> Maybe.map (flip appendLog calc)


parse : String -> Calculus ctx t ty -> Result (List Parser.DeadEnd) (Calculus ctx t ty)
parse s calc =
    calc.parse s
        |> Result.map (flip appendLog (initLog calc))


appendLog : t -> Calculus ctx t ty -> Calculus ctx t ty
appendLog t calc =
    { calc | logs = t :: calc.logs }


initLog : Calculus ctx t ty -> Calculus ctx t ty
initLog calc =
    { calc | logs = [] }


display : Calculus ctx t ty -> List String
display calc =
    List.map calc.display calc.logs


typecheck : Calculus ctx t ty -> Bool
typecheck calc =
    case ( calc.typeof, List.head calc.logs ) of
        ( Just f, Just t ) ->
            f calc.init t
                |> Maybe.map (always True)
                |> Maybe.withDefault False

        _ ->
            True
