module TaPL.Chap7Test exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Parser
import TaPL.Chap7 as Chap7
import TaPL.Chap7.Parser as Chap7
import Test exposing (..)


suite : Test
suite =
    describe "The TaPL.Chap7 and TaPL.Chap7.Parser module"
        [ describe "eval1 function"
            (let
                initCtx =
                    []
             in
             [ test "success case" <|
                \_ ->
                    Chap7.eval1 initCtx term1
                        |> Expect.equal (Just term2)
             , test "failure case" <|
                \_ ->
                    Chap7.eval1 initCtx term2
                        |> Expect.equal Nothing
             ]
            )
        , describe "parse function"
            [ test "success case 1" <|
                \_ ->
                    Chap7.parse "(\\x . \\f . f x) (\\x . x)"
                        |> Expect.equal (Ok term1)
            , test "success case 2" <|
                \_ ->
                    Chap7.parse "\\x . \\x . x x"
                        |> Expect.equal (Ok term3)
            , test "failure case" <|
                \_ ->
                    Chap7.parse "\\x . \\f . f y"
                        |> Expect.equal (Err [ Parser.DeadEnd 1 14 (Parser.Problem "undefined variable: y") ])
            ]
        , describe "display function"
            [ test "success case 1" <|
                \_ -> Chap7.display term1 |> Expect.equal "(\\x. (\\f. (f x))) (\\x. x)"
            , test "success case 2" <|
                \_ -> Chap7.display term3 |> Expect.equal "\\x. (\\x'. (x' x'))"
            , test "failure case" <|
                \_ -> Chap7.display term4 |> Expect.equal ""
            ]
        ]


term1 : Chap7.Term
term1 =
    Chap7.TmApp
        (Chap7.TmAbs "x"
            (Chap7.TmAbs "f"
                (Chap7.TmApp (Chap7.TmVar 0 2) (Chap7.TmVar 1 2))
            )
        )
        (Chap7.TmAbs "x" (Chap7.TmVar 0 1))


term2 : Chap7.Term
term2 =
    Chap7.TmAbs "f"
        (Chap7.TmApp
            (Chap7.TmVar 0 1)
            (Chap7.TmAbs "x" (Chap7.TmVar 0 2))
        )


term3 : Chap7.Term
term3 =
    Chap7.TmAbs "x"
        (Chap7.TmAbs "x"
            (Chap7.TmApp (Chap7.TmVar 0 2) (Chap7.TmVar 0 2))
        )


term4 : Chap7.Term
term4 =
    Chap7.TmAbs "x"
        (Chap7.TmAbs "x"
            (Chap7.TmApp (Chap7.TmVar 0 1) (Chap7.TmVar 0 2))
        )
