module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html as Html exposing (..)
import Html.Attributes exposing (attribute, class, href, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Parser
import TaPL
import Url exposing (Url)
import Url.Parser as Url exposing ((</>), (<?>))
import Url.Parser.Query as UrlQuery


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


type alias Model =
    { key : Nav.Key
    , url : Url
    , input : String
    , error : String
    , chap : TaPL.Chapter
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url "" "" TaPL.Chap0
        |> updateModelWithUrl
    , Cmd.none
    )


type Msg
    = OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | InputText String
    | SelectChap TaPL.Chapter
    | ParseInput (Result (List Parser.DeadEnd) TaPL.Chapter)
    | EvalTerm (Maybe TaPL.Chapter)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlRequest (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        OnUrlRequest (Browser.External link) ->
            ( model, Nav.load link )

        OnUrlChange url ->
            ( updateModelWithUrl { model | url = url }, Cmd.none )

        InputText txt ->
            ( { model | input = txt }, Cmd.none )

        SelectChap chap ->
            ( { model | chap = chap }, Cmd.none )

        ParseInput (Ok chap) ->
            if TaPL.typecheck chap then
                ( { model | chap = chap, error = "" }, Cmd.none )

            else
                ( { model | chap = TaPL.init (TaPL.toString chap), error = "Invalid type" }, Cmd.none )

        ParseInput (Err _) ->
            ( { model | chap = TaPL.init (TaPL.toString model.chap), error = "Can not parse" }, Cmd.none )

        EvalTerm (Just chap) ->
            ( { model | chap = chap }, Cmd.none )

        EvalTerm _ ->
            ( { model | error = "Can not eval" }, Cmd.none )


updateModelWithUrl : Model -> Model
updateModelWithUrl model =
    let
        url =
            model.url
    in
    case Url.parse parser { url | path = "" } of
        Just query ->
            { model | chap = query.chap, input = query.exp }

        _ ->
            model


type alias Query =
    { chap : TaPL.Chapter, exp : String }


parser : Url.Parser (Query -> a) a
parser =
    let
        chapParesr =
            UrlQuery.custom "chap" <|
                \xs ->
                    List.head xs
                        |> Maybe.map TaPL.init
                        |> Maybe.withDefault TaPL.Chap0

        expParser =
            UrlQuery.string "exp"
                |> UrlQuery.map (Maybe.withDefault "")
    in
    Url.top <?> UrlQuery.map2 Query chapParesr expParser


title : String
title =
    "TaPL's Lambda calculus"


view : Model -> Browser.Document Msg
view model =
    let
        link =
            String.concat
                [ "?chap="
                , TaPL.toString model.chap
                , "&exp="
                , model.input
                ]
    in
    { title = title
    , body =
        [ div [ class "Box text-center my-3 container-sm" ]
            [ div [ class "Box-header" ]
                [ h1 [ class "Box-title" ]
                    [ a [ href link ] [ text title ] ]
                ]
            , viewBody model
            ]
        ]
    }


viewBody : Model -> Html Msg
viewBody model =
    div [ class "Box-Body" ]
        [ pre [ class "text-left d-flex flex-justify-center" ]
            [ text (TaPL.syntax model.chap) ]
        , button
            [ onClick (ParseInput <| TaPL.parse model.chap model.input)
            , class "btn my-2"
            , type_ "button"
            ]
            [ text "Parse!" ]
        , input
            [ onInput InputText
            , class "form-control"
            , type_ "text"
            , placeholder "lambda calculus"
            , value model.input
            ]
            []
        , select
            [ onInput (SelectChap << TaPL.init)
            , class "form-select"
            , attribute "aria-label" "Important decision"
            ]
            (option [] [ text "Select" ]
                :: List.map (\s -> option [ value s ] [ text s ]) TaPL.chapters
            )
        , div [] (viewEnv model)
        , if String.isEmpty model.error then
            div [] []

          else
            div [ class "flash flash-error" ] [ text model.error ]
        ]


viewEnv : Model -> List (Html Msg)
viewEnv model =
    case TaPL.display model.chap of
        [] ->
            []

        logs ->
            [ List.reverse logs
                |> List.map (\log -> div [ class "my-1" ] [ text log ])
                |> List.intersperse (div [ class "my-1" ] [ text "↓" ])
                |> div []
            , button
                [ onClick (EvalTerm <| TaPL.eval1 model.chap)
                , class "btn btn-sm my-2"
                , type_ "button"
                ]
                [ text "Eval!" ]
            ]
