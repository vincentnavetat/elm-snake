module Main exposing (init, main, update)

import Browser
import Keyboard exposing (KeyboardConfig, subscription)
import Model exposing (Cell, Direction(..), Model, Msg(..), Status(..), mapSize)
import Time
import View exposing (view)


initSnake : Int -> List Cell
initSnake size =
    List.range 1 size
        |> List.map (\x -> { x = x, y = 1 })
        |> List.reverse


init : Int -> ( Model, Cmd Msg )
init _ =
    ( { snake = initSnake 10
      , direction = Right
      , status = OnGoing
      }
    , Cmd.none
    )


cellIsInMap : Cell -> Bool
cellIsInMap c =
    c.x > 0 && c.x <= mapSize && c.y > 0 && c.y <= mapSize


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

        snakeHead =
            List.head model.snake
                |> Maybe.withDefault { x = 1, y = 1 }

        newHead =
            updateCells model.direction snakeHead

        newSnake =
            newHead :: List.take (List.length model.snake - 1) model.snake

        ( newStatus, snakeToReturn ) =
            if List.all cellIsInMap newSnake then
                ( OnGoing, newSnake )

            else
                ( GameOver, model.snake )
    in
    { model
        | snake = snakeToReturn
        , status = newStatus
    }


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
