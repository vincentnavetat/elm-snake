module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import Time


type alias Cell =
    { x : Int
    , y : Int
    }


type Status
    = OnGoing
    | GameOver


type alias Model =
    { snake : List Cell
    , status : Status
    }


type Msg
    = TimeTick Time.Posix


init : Int -> ( Model, Cmd Msg )
init _ =
    ( { snake = [ { x = 1, y = 1 } ]
      , status = OnGoing
      }
    , Cmd.none
    )


moveSnake : Model -> Model
moveSnake model =
    let
        updateCells c =
            { c | x = c.x + 1 }

        newSnake =
            List.map updateCells model.snake

        ( newStatus, snakeToReturn ) =
            if List.any (\c -> c.x > size) newSnake then
                ( GameOver, model.snake )

            else
                ( OnGoing, newSnake )
    in
    { model | snake = snakeToReturn, status = newStatus }


play : Model -> ( Model, Cmd Msg )
play model =
    case model.status of
        OnGoing ->
            ( model |> moveSnake, Cmd.none )

        GameOver ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        TimeTick _ ->
            play model


size : Int
size =
    10


drawCols : Model -> List (Html Msg)
drawCols model =
    List.range 1 size
        |> List.map (drawRow model)


drawRow : Model -> Int -> Html Msg
drawRow model y =
    let
        row =
            List.range 1 size
                |> List.map (\x -> drawCell model { x = x, y = y })
    in
    div [ class "row" ]
        row


drawCell : Model -> Cell -> Html Msg
drawCell model cell =
    let
        isSnake =
            List.member cell model.snake
    in
    div [ class "cell", classList [ ( "cell--snake", isSnake ) ] ]
        [ cell.x |> String.fromInt |> text
        , text " - "
        , cell.y |> String.fromInt |> text
        ]


viewGameStatus : Status -> Html Msg
viewGameStatus s =
    if s == GameOver then
        text "Game over looooser!"

    else
        text ""


view : Model -> Html Msg
view model =
    node "main"
        []
        [ div [ class "map" ]
            (drawCols model)
        , div []
            [ viewGameStatus model.status
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 TimeTick


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Snake!"
                , body = [ view m ]
                }
        , subscriptions = subscriptions
        }
