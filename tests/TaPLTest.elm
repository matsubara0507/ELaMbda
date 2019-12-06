module TaPLTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import TaPL
import TaPL.Calculus as Calculus
import TaPL.Chap4 as Chap4
import TaPL.Chap7 as Chap7
import Test exposing (..)


suite : Test
suite =
    describe "The TaPL module"
        [ describe "Convert Chapter and String functions"
            [ fuzz fuzzChapter "Equivarence Chapter to/from String" <|
                \chapStr ->
                    TaPL.toString (TaPL.init chapStr)
                        |> Expect.equal chapStr
            ]
        , describe "eval1 function"
            [ describe "when Chapter is Chap0"
                [ test "should return Nothing" <|
                    \_ ->
                        TaPL.eval1 TaPL.Chap0
                            |> Expect.equal Nothing
                ]
            , describe "when Chapter is Chap4"
                [ test "should return Chap4" <|
                    \_ -> TaPL.eval1 exChap4 |> shouldReturnSameChapterM exChap4
                ]
            , describe "when Chapter is Chap7"
                [ test "should return Chap7" <|
                    \_ -> TaPL.eval1 exChap7 |> shouldReturnSameChapterM exChap7
                ]
            ]
        , describe "parse function"
            [ describe "when Chapter is Chap0"
                [ test "should return Err []" <|
                    \_ ->
                        TaPL.parse TaPL.Chap0 "double"
                            |> Expect.equal (Err [])
                ]
            , describe "when Chapter is Chap4"
                [ test "should return Chap4" <|
                    \_ -> TaPL.parse exChap4 "true" |> shouldReturnSameChapterR exChap4
                ]
            , describe "when Chapter is Chap7"
                [ test "should return Chap7" <|
                    \_ -> TaPL.parse exChap7 "\\x. x" |> shouldReturnSameChapterR exChap7
                ]
            ]
        ]


fuzzChapter : Fuzzer String
fuzzChapter =
    "chap0"
        :: TaPL.chapters
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


shouldReturnSameChapterM : TaPL.Chapter -> Maybe TaPL.Chapter -> Expectation
shouldReturnSameChapterM expected actual =
    actual
        |> Maybe.map TaPL.toString
        |> Expect.equal (Just <| TaPL.toString expected)


shouldReturnSameChapterR : TaPL.Chapter -> Result e TaPL.Chapter -> Expectation
shouldReturnSameChapterR expected actual =
    actual
        |> Result.map TaPL.toString
        |> Expect.equal (Ok <| TaPL.toString expected)


exChap4 : TaPL.Chapter
exChap4 =
    examples "chap4"


exChap7 : TaPL.Chapter
exChap7 =
    examples "chap7"


examples : String -> TaPL.Chapter
examples s =
    case TaPL.init s of
        TaPL.Chap4 calc ->
            calc
                |> Calculus.appendLog (Chap4.TmPred Chap4.TmZero)
                |> TaPL.Chap4

        TaPL.Chap7 calc ->
            calc
                |> Calculus.appendLog (Chap7.TmApp (Chap7.TmAbs "x" (Chap7.TmVar 0 1)) (Chap7.TmAbs "x" (Chap7.TmVar 0 1)))
                |> TaPL.Chap7

        _ ->
            TaPL.Chap0
