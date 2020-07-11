module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard exposing (KeyboardConfig, subscription)
import Time


type Status
    = OnGoing
    | GameOver


type Direction
    = Up
    | Right
    | Down
    | Left


type alias Cell =
    { x : Int
    , y : Int
    }


type alias Model =
    { snake : List Cell
    , direction : Direction
    , status : Status
    }


type Msg
    = TimeTick Time.Posix
    | ChangeDirection Direction
    | NoOp


init : Int -> ( Model, Cmd Msg )
init _ =
    ( { snake = [ { x = 1, y = 1 } ]
      , direction = Right
      , status = OnGoing
      }
    , Cmd.none
    )


cellIsInMap : Cell -> Bool
cellIsInMap c =
    c.x > 0 && c.x <= size && c.y > 0 && c.y <= size


moveSnake : Model -> Model
moveSnake model =
    let
        updateCells direction c =
            case direction of
                Up ->
                    { c | y = c.y - 1 }

                Right ->
                    { c | x = c.x + 1 }

                Down ->
                    { c | y = c.y + 1 }

                Left ->
                    { c | x = c.x - 1 }

        newSnake =
            model.snake
                |> List.map (updateCells model.direction)

        ( newStatus, snakeToReturn ) =
            if List.all cellIsInMap newSnake then
                ( OnGoing, newSnake )

            else
                ( GameOver, model.snake )
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

        ChangeDirection d ->
            ( { model | direction = d }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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


keyboardConfig : KeyboardConfig Msg
keyboardConfig =
    { up = ChangeDirection Up
    , right = ChangeDirection Right
    , down = ChangeDirection Down
    , left = ChangeDirection Left
    , noop = NoOp
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ subscription keyboardConfig
        , Time.every 200 TimeTick
        ]


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
