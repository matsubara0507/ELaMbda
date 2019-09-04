module TaPL.Chap4Test exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Parser
import TaPL.Chap4 as Chap4
import TaPL.Chap4.Parser as Chap4
import Test exposing (..)


suite : Test
suite =
    describe "The TaPL.Chap4 and TaPL.Chap4.Parser module"
        [ describe "eval1 function"
            [ test "success case" <|
                \_ ->
                    Maybe.andThen Chap4.eval1 (Chap4.eval1 term1)
                        |> Expect.equal (Just term2)
            , test "failure case" <|
                \_ ->
                    Chap4.eval1 term3
                        |> Expect.equal Nothing
            ]
        , describe "parse function"
            [ test "success case 1" <|
                \_ ->
                    Chap4.parse "pred (if iszero 0 then 2 else 1)"
                        |> Expect.equal (Ok term1)
            , test "success case 2" <|
                \_ ->
                    Chap4.parse "pred (if 0 then succ 1 else 1)"
                        |> Expect.equal (Ok term3)
            , test "failure case" <|
                \_ ->
                    Result.toMaybe (Chap4.parse "hoge")
                        |> Expect.equal Nothing
            ]
        , describe "display function"
            [ test "allways success" <|
                \_ -> Chap4.display term1 |> Expect.equal "pred (if (iszero 0) then 2 else 1)"
            ]
        ]


term1 : Chap4.Term
term1 =
    Chap4.TmPred (Chap4.TmIf (Chap4.TmIsZero Chap4.TmZero) (Chap4.TmSucc (Chap4.TmSucc Chap4.TmZero)) (Chap4.TmSucc Chap4.TmZero))


term2 : Chap4.Term
term2 =
    Chap4.TmPred (Chap4.TmSucc (Chap4.TmSucc Chap4.TmZero))


term3 : Chap4.Term
term3 =
    Chap4.TmPred (Chap4.TmIf Chap4.TmZero (Chap4.TmSucc (Chap4.TmSucc Chap4.TmZero)) (Chap4.TmSucc Chap4.TmZero))
