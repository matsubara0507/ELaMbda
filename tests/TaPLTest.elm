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
                \chap ->
                    TaPL.chapterFromString (TaPL.chapterToString chap)
                        |> Expect.equal chap
            ]
        , describe "eval1 function"
            (let
                shouldChange f chap =
                    Maybe.map f (TaPL.eval1 chap mockModel)
                        |> Expect.notEqual (Just (f mockModel))

                shouldNotChange f chap =
                    Maybe.map f (TaPL.eval1 chap mockModel)
                        |> Expect.equal (Just (f mockModel))
             in
             [ describe "when Chapter is Chap0"
                [ test "should return Nothing" <|
                    \_ ->
                        TaPL.eval1 TaPL.Chap0 mockModel
                            |> Expect.equal Nothing
                ]
             , describe "when Chapter is Chap4"
                [ test "should change .chap4" <|
                    \_ -> shouldChange .chap4 TaPL.Chap4
                , test "should not change .chap7" <|
                    \_ -> shouldNotChange .chap7 TaPL.Chap4
                ]
             , describe "when Chapter is Chap7"
                [ test "should not change .chap4" <|
                    \_ -> shouldNotChange .chap4 TaPL.Chap7
                , test "should  change .chap7" <|
                    \_ -> shouldChange .chap7 TaPL.Chap7
                ]
             ]
            )
        , describe "parse function"
            (let
                shouldChange f chap =
                    Result.map f (TaPL.parse chap "double" mockModel)
                        |> Expect.notEqual (Ok (f mockModel))

                shouldNotChange f chap =
                    Result.map f (TaPL.parse chap "double" mockModel)
                        |> Expect.equal (Ok (f mockModel))
             in
             [ describe "when Chapter is Chap0"
                [ test "should return Err []" <|
                    \_ ->
                        TaPL.parse TaPL.Chap0 "double" mockModel
                            |> Expect.equal (Err [])
                ]
             , describe "when Chapter is Chap4"
                [ test "should change .chap4" <|
                    \_ -> shouldChange .chap4 TaPL.Chap4
                , test "should not change .chap7" <|
                    \_ -> shouldNotChange .chap7 TaPL.Chap4
                ]
             , describe "when Chapter is Chap7"
                [ test "should not change .chap4" <|
                    \_ -> shouldNotChange .chap4 TaPL.Chap7
                , test "should  change .chap7" <|
                    \_ -> shouldChange .chap7 TaPL.Chap7
                ]
             ]
            )
        ]


fuzzChapter : Fuzzer TaPL.Chapter
fuzzChapter =
    [ TaPL.Chap0, TaPL.Chap4, TaPL.Chap7 ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


mockModel : TaPL.Model
mockModel =
    let
        chap4 =
            Calculus.appendLog Chap4.TmTrue TaPL.init.chap4

        chap7 =
            Calculus.appendLog (Chap7.TmAbs "x" (Chap7.TmVar 0 1)) TaPL.init.chap7
    in
    { chap4 =
        { chap4 | eval1 = \_ t -> Just t, parse = \_ -> Ok Chap4.TmZero }
    , chap7 =
        { chap7 | eval1 = \_ t -> Just t, parse = \_ -> Ok (Chap7.TmVar 0 0) }
    }
