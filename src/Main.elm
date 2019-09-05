module Main exposing (main)

import Browser
import Html as Html exposing (..)
import Html.Attributes exposing (attribute, class, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Parser
import TaPL


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { input : String
    , error : String
    , chap : TaPL.Chapter
    , env : TaPL.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" TaPL.Chap0 TaPL.init, Cmd.none )


type Msg
    = InputText String
    | SelectChap TaPL.Chapter
    | ParseInput (Result (List Parser.DeadEnd) TaPL.Model)
    | EvalTerm (Maybe TaPL.Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputText txt ->
            ( { model | input = txt }, Cmd.none )

        SelectChap chap ->
            ( { model | chap = chap, env = TaPL.init }, Cmd.none )

        ParseInput (Ok env) ->
            ( { model | env = env, error = "" }, Cmd.none )

        ParseInput (Err _) ->
            ( { model | env = TaPL.init, error = "Can not parse" }, Cmd.none )

        EvalTerm (Just env) ->
            ( { model | env = env }, Cmd.none )

        EvalTerm _ ->
            ( { model | error = "Can not eval" }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ pre [ class "text-left d-flex flex-justify-center" ]
            [ text (TaPL.syntax model.chap model.env) ]
        , button
            [ onClick (ParseInput <| TaPL.parse model.chap model.input model.env)
            , class "btn my-2"
            , type_ "button"
            ]
            [ text "Parse!" ]
        , input
            [ onInput InputText
            , class "form-control"
            , type_ "text"
            , placeholder "lambda calculus"
            ]
            []
        , select
            [ onInput (SelectChap << TaPL.chapterFromString)
            , class "form-select"
            , attribute "aria-label" "Important decision"
            ]
            [ option [] [ text "Select" ]
            , option [ value (TaPL.chapterToString TaPL.Chap4) ] [ text (TaPL.chapterToString TaPL.Chap4) ]
            , option [ value (TaPL.chapterToString TaPL.Chap7) ] [ text (TaPL.chapterToString TaPL.Chap7) ]
            ]
        , div [] (viewEnv model)
        , if String.isEmpty model.error then
            div [] []

          else
            div [ class "flash flash-error" ] [ text model.error ]
        ]


viewEnv : Model -> List (Html Msg)
viewEnv model =
    case TaPL.display model.chap model.env of
        [] ->
            []

        logs ->
            [ List.reverse logs
                |> List.map (\log -> div [ class "my-1" ] [ text log ])
                |> List.intersperse (div [ class "my-1" ] [ text "↓" ])
                |> div []
            , button
                [ onClick (EvalTerm <| TaPL.eval1 model.chap model.env)
                , class "btn btn-sm my-2"
                , type_ "button"
                ]
                [ text "Eval!" ]
            ]
