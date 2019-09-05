module TaPL.Chap4 exposing (Term(..), display, eval, eval1, fromInt, isnumericval, isval, syntax, toInt)


syntax : String
syntax =
    """
t  := v
    | if t then t else t
    | succ t
    | pred t
    | iszero t

v  := true | false | nv
nv := [natural number]
    """


type Term
    = TmTrue
    | TmFalse
    | TmIf Term Term Term
    | TmZero
    | TmSucc Term
    | TmPred Term
    | TmIsZero Term


isnumericval : Term -> Bool
isnumericval t =
    case t of
        TmZero ->
            True

        TmSucc t1 ->
            isnumericval t1

        _ ->
            False


isval : Term -> Bool
isval t =
    case t of
        TmTrue ->
            True

        TmFalse ->
            True

        _ ->
            isnumericval t


eval1 : Term -> Maybe Term
eval1 t =
    case t of
        TmIf TmTrue t2 _ ->
            Just t2

        TmIf TmFalse _ t3 ->
            Just t3

        TmIf t1 t2 t3 ->
            eval1 t1 |> Maybe.map (\t1_ -> TmIf t1_ t2 t3)

        TmSucc t1 ->
            eval1 t1 |> Maybe.map TmSucc

        TmPred TmZero ->
            Just TmZero

        TmPred (TmSucc nv1) ->
            if isnumericval nv1 then
                Just nv1

            else
                eval1 (TmSucc nv1) |> Maybe.map TmPred

        TmPred t1 ->
            eval1 t1 |> Maybe.map TmPred

        TmIsZero TmZero ->
            Just TmTrue

        TmIsZero (TmSucc nv1) ->
            if isnumericval nv1 then
                Just TmFalse

            else
                eval1 (TmSucc nv1) |> Maybe.map TmIsZero

        TmIsZero t1 ->
            eval1 t1 |> Maybe.map TmIsZero

        _ ->
            Nothing


eval : Term -> Maybe Term
eval t =
    if isval t then
        Just t

    else
        Maybe.andThen eval (eval1 t)


fromInt : Int -> Term
fromInt n =
    if n > 0 then
        TmSucc (fromInt (n - 1))

    else
        TmZero


toInt : Term -> Maybe Int
toInt t =
    case t of
        TmZero ->
            Just 0

        TmSucc t1 ->
            Maybe.map (\n -> 1 + n) (toInt t1)

        _ ->
            Nothing


display : Term -> String
display t =
    displayR t
        |> dropIfStartsWith "("
        |> dropIfEndsWith ")"


displayR : Term -> String
displayR t =
    case ( toInt t, t ) of
        ( Just n, _ ) ->
            String.fromInt n

        ( _, TmTrue ) ->
            "true"

        ( _, TmFalse ) ->
            "false"

        ( _, TmIf t1 t2 t3 ) ->
            String.concat
                [ "(if "
                , displayR t1
                , " then "
                , displayR t2
                , " else "
                , displayR t3
                , ")"
                ]

        ( _, TmZero ) ->
            "0"

        ( _, TmPred t1 ) ->
            String.concat [ "(pred ", displayR t1, ")" ]

        ( _, TmSucc t1 ) ->
            String.concat [ "(succ ", displayR t1, ")" ]

        ( _, TmIsZero t1 ) ->
            String.concat [ "(iszero ", displayR t1, ")" ]


dropIfStartsWith : String -> String -> String
dropIfStartsWith word s =
    if String.startsWith word s then
        String.dropLeft (String.length word) s

    else
        s


dropIfEndsWith : String -> String -> String
dropIfEndsWith word s =
    if String.endsWith word s then
        String.dropRight (String.length word) s

    else
        s
