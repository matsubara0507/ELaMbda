module TaPL.Chap7 exposing (Binding(..), Context, Term(..), ctxlength, display, eval, eval1, index2name, isval, pickfreshname, printtm, syntax, termShift, termSubst, termSubstTop)

import Basics.Extra exposing (flip)
import Debug
import List.Extra as List
import TaPL.Utils exposing (..)


syntax : String
syntax =
    """
t := x        [variable]
   | \\x . t   [abstraction]
   | t t      [application]
    """


type Term
    = TmVar Int Int
    | TmAbs String Term
    | TmApp Term Term


type alias Context =
    List ( String, Binding )


type Binding
    = NameBind


display : Term -> String
display t =
    printtm [] t
        |> Maybe.map (dropIfStartsWith "(")
        |> Maybe.map (dropIfEndsWith ")")
        |> Maybe.withDefault ""


printtm : Context -> Term -> Maybe String
printtm ctx t =
    case t of
        TmAbs x t1 ->
            let
                ( ctx1, x1 ) =
                    pickfreshname ctx x
            in
            Maybe.map
                (\s1 -> String.concat [ "(\\", x1, ". ", s1, ")" ])
                (printtm ctx1 t1)

        TmApp t1 t2 ->
            Maybe.map2
                (\s1 s2 -> String.concat [ "(", s1, " ", s2, ")" ])
                (printtm ctx t1)
                (printtm ctx t2)

        TmVar x n ->
            if ctxlength ctx == n then
                index2name ctx x

            else
                Nothing


pickfreshname : Context -> String -> ( Context, String )
pickfreshname ctx x =
    let
        x1 =
            ctx
                |> List.map Tuple.first
                |> List.filter (String.startsWith x)
                |> List.maximum
                |> Maybe.map (\a -> a ++ "'")
                |> Maybe.withDefault x
    in
    ( ( x1, NameBind ) :: ctx, x1 )


ctxlength : Context -> Int
ctxlength ctx =
    List.length ctx


index2name : Context -> Int -> Maybe String
index2name ctx x =
    case List.getAt x ctx of
        Just ( str, _ ) ->
            Just str

        _ ->
            Nothing


termShift : Int -> Term -> Term
termShift d =
    let
        walk c t =
            case t of
                TmVar x n ->
                    if x >= c then
                        TmVar (x + d) (n + d)

                    else
                        TmVar x (n + d)

                TmAbs x t1 ->
                    TmAbs x (walk (c + 1) t1)

                TmApp t1 t2 ->
                    TmApp (walk c t1) (walk c t2)
    in
    walk 0


termSubst : Int -> Term -> Term -> Term
termSubst j s =
    let
        walk c t =
            case t of
                TmVar x n ->
                    if x == j + c then
                        termShift c s

                    else
                        TmVar x n

                TmAbs x t1 ->
                    TmAbs x (walk (c + 1) t1)

                TmApp t1 t2 ->
                    TmApp (walk c t1) (walk c t2)
    in
    walk 0


termSubstTop : Term -> Term -> Term
termSubstTop s t =
    termShift -1 (termSubst 0 (termShift 1 s) t)


isval : Context -> Term -> Bool
isval _ t =
    case t of
        TmAbs _ _ ->
            True

        _ ->
            False


eval1 : Context -> Term -> Maybe Term
eval1 ctx t =
    case t of
        TmApp (TmAbs x t12) t2 ->
            if isval ctx t2 then
                Just (termSubstTop t2 t12)

            else
                Maybe.map (TmApp (TmAbs x t12)) (eval1 ctx t2)

        TmApp t1 t2 ->
            Maybe.map (flip TmApp t2) (eval1 ctx t1)

        _ ->
            Nothing


eval : Context -> Term -> Maybe Term
eval ctx t =
    if isval ctx t then
        Just t

    else
        Maybe.andThen (eval ctx) (eval1 ctx t)
