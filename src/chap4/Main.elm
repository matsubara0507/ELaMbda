module Main exposing (main)

import Browser
import Html as Html exposing (..)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Lambda exposing (Term(..))
import Lambda.Parser as Lambda
import Parser


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
    , exps : List Lambda.Term
    , error : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" [] "", Cmd.none )


type Msg
    = InputText String
    | ParseInput (Result (List Parser.DeadEnd) Term)
    | EvalTerm (Maybe Term)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputText txt ->
            ( { model | input = txt }, Cmd.none )

        ParseInput (Ok t) ->
            ( { model | exps = [ t ], error = "" }, Cmd.none )

        ParseInput (Err _) ->
            ( { model | exps = [], error = "Can not parse" }, Cmd.none )

        EvalTerm (Just t) ->
            ( { model | exps = t :: model.exps }, Cmd.none )

        EvalTerm _ ->
            ( { model | error = "Can not eval" }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ button
            [ onClick (ParseInput <| Lambda.parse model.input)
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
        , div [] (viewExps model)
        , if String.isEmpty model.error then
            div [] []

          else
            div [ class "flash flash-error" ] [ text model.error ]
        ]


viewExps : Model -> List (Html Msg)
viewExps model =
    case model.exps of
        [] ->
            []

        x :: xs ->
            [ List.reverse model.exps
                |> List.map viewExp
                |> List.intersperse (div [ class "my-1" ] [ text "↓" ])
                |> div []
            , button
                [ onClick (EvalTerm <| Lambda.eval1 x)
                , class "btn btn-sm my-2"
                , type_ "button"
                ]
                [ text "Eval!" ]
            ]


viewExp : Lambda.Term -> Html Msg
viewExp t =
    div [ class "my-1" ] [ text (Lambda.display t) ]
