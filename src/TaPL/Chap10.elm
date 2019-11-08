module TaPL.Chap10 exposing (Binding(..), Context, Term(..), Ty(..), addbinding, ctxlength, display, eval, eval1, getTypeFromContext, getbinding, index2name, isval, pickfreshname, printtm, syntax, termShift, termSubst, termSubstTop, typeof)

import Basics.Extra exposing (flip)
import List.Extra as List
import TaPL.Utils exposing (..)


syntax : String
syntax =
    """
t := x            [variable]
   | \\x : T. t    [abstraction]
   | t t          [application]
   | true
   | false
   | if t then t else t

T := Bool
   | T -> T
    """


type Ty
    = TyArr Ty Ty
    | TyBool


type Term
    = TmVar Int Int
    | TmAbs String Ty Term
    | TmApp Term Term
    | TmTrue
    | TmFalse
    | TmIf Term Term Term


type alias Context =
    List ( String, Binding )


type Binding
    = NameBind
    | VarBind Ty


addbinding : Context -> String -> Binding -> Context
addbinding ctx x bind =
    ( x, bind ) :: ctx


getbinding : Context -> Int -> Maybe Binding
getbinding ctx n =
    if n <= 0 then
        List.head ctx |> Maybe.map Tuple.second

    else
        List.tail ctx |> Maybe.andThen (\c -> getbinding c (n - 1))


getTypeFromContext : Context -> Int -> Maybe Ty
getTypeFromContext ctx idx =
    case getbinding ctx idx of
        Just (VarBind ty) ->
            Just ty

        _ ->
            Nothing


display : Term -> String
display t =
    printtm [] t
        |> Maybe.map (dropIfStartsWith "(")
        |> Maybe.map (dropIfEndsWith ")")
        |> Maybe.withDefault ""


printtm : Context -> Term -> Maybe String
printtm ctx t =
    case t of
        TmAbs x ty t1 ->
            let
                ( ctx1, x1 ) =
                    pickfreshname ctx x
            in
            Maybe.map
                (\s1 -> String.concat [ "(\\", x1, " : ", printty ty, ". ", s1, ")" ])
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

        TmTrue ->
            Just "true"

        TmFalse ->
            Just "false"

        TmIf t1 t2 t3 ->
            Maybe.map3
                (\s1 s2 s3 -> String.concat [ "(if ", s1, " then ", s2, " else ", s3, ")" ])
                (printtm ctx t1)
                (printtm ctx t2)
                (printtm ctx t3)


printty : Ty -> String
printty ty =
    case ty of
        TyArr ty1 ty2 ->
            String.concat [ "(", printty ty1, " -> ", printty ty2, ")" ]

        TyBool ->
            "Bool"


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

                TmAbs x ty t1 ->
                    TmAbs x ty (walk (c + 1) t1)

                TmApp t1 t2 ->
                    TmApp (walk c t1) (walk c t2)

                TmIf t1 t2 t3 ->
                    TmIf (walk c t1) (walk c t2) (walk c t3)

                _ ->
                    t
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

                TmAbs x ty t1 ->
                    TmAbs x ty (walk (c + 1) t1)

                TmApp t1 t2 ->
                    TmApp (walk c t1) (walk c t2)

                TmIf t1 t2 t3 ->
                    TmIf (walk c t1) (walk c t2) (walk c t3)

                _ ->
                    t
    in
    walk 0


termSubstTop : Term -> Term -> Term
termSubstTop s t =
    termShift -1 (termSubst 0 (termShift 1 s) t)


isval : Context -> Term -> Bool
isval _ t =
    case t of
        TmAbs _ _ _ ->
            True

        TmTrue ->
            True

        TmFalse ->
            True

        _ ->
            False


eval1 : Context -> Term -> Maybe Term
eval1 ctx t =
    case t of
        TmApp (TmAbs x ty t12) t2 ->
            if isval ctx t2 then
                Just (termSubstTop t2 t12)

            else
                Maybe.map (TmApp (TmAbs x ty t12)) (eval1 ctx t2)

        TmApp t1 t2 ->
            Maybe.map (flip TmApp t2) (eval1 ctx t1)

        TmIf TmTrue t2 _ ->
            Just t2

        TmIf TmFalse _ t3 ->
            Just t3

        TmIf t1 t2 t3 ->
            eval1 ctx t1 |> Maybe.map (\t1_ -> TmIf t1_ t2 t3)

        _ ->
            Nothing


eval : Context -> Term -> Maybe Term
eval ctx t =
    if isval ctx t then
        Just t

    else
        Maybe.andThen (eval ctx) (eval1 ctx t)


typeof : Context -> Term -> Maybe Ty
typeof ctx t =
    case t of
        TmVar x _ ->
            getTypeFromContext ctx x

        TmAbs x ty1 t2 ->
            let
                ctx1 =
                    addbinding ctx x (VarBind ty1)
            in
            typeof ctx1 t2
                |> Maybe.map (\ty2 -> TyArr ty1 ty2)

        TmApp t1 t2 ->
            case ( typeof ctx t1, typeof ctx t2 ) of
                ( Just (TyArr ty11 ty12), Just ty2 ) ->
                    if ty11 == ty2 then
                        Just ty12

                    else
                        Nothing

                _ ->
                    Nothing

        TmTrue ->
            Just TyBool

        TmFalse ->
            Just TyBool

        TmIf t1 t2 t3 ->
            case ( typeof ctx t1, typeof ctx t2, typeof ctx t3 ) of
                ( Just TyBool, Just ty2, Just ty3 ) ->
                    if ty2 == ty3 then
                        Just ty2

                    else
                        Nothing

                _ ->
                    Nothing
