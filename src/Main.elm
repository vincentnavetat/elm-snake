module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Cell =
    { x : Int
    , y : Int
    }


type alias Model =
    { snake : List Cell
    }


init : Int -> ( Model, Cmd Msg )
init _ =
    ( { snake = [ { x = 1, y = 1 } ] }, Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )


size : Int
size =
    10


drawCols : Model -> List (Html Msg)
drawCols model =
    let
        cols =
            List.range 1 size
    in
    List.map (drawRow model) cols


drawRow : Model -> Int -> Html Msg
drawRow model y =
    let
        row =
            List.range 1 size
    in
    div [ class "row" ]
        (List.map (drawCell model y) row)


drawCell : Model -> Int -> Int -> Html Msg
drawCell model x y =
    let
        isSnake =
            List.member { x = x, y = y } model.snake
    in
    div [ class "cell", classList [ ( "cell--snake", isSnake ) ] ]
        [ x |> String.fromInt |> text
        , text " - "
        , y |> String.fromInt |> text
        ]


view : Model -> Html Msg
view model =
    div [ class "map" ]
        (drawCols model)


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Elm 0.19 starter"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }
