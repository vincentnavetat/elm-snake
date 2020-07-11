module Main exposing (init, main, update)

import Browser
import Keyboard exposing (KeyboardConfig, subscription)
import Model exposing (Cell, Direction(..), Model, Msg(..), Status(..), mapSize, sameCell)
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


moveUp : Cell -> Cell
moveUp c =
    if c.y <= 1 then
        { c | y = mapSize }

    else
        { c | y = c.y - 1 }


moveDown : Cell -> Cell
moveDown c =
    if c.y >= mapSize then
        { c | y = 1 }

    else
        { c | y = c.y + 1 }


moveLeft : Cell -> Cell
moveLeft c =
    if c.x <= 1 then
        { c | x = mapSize }

    else
        { c | x = c.x - 1 }


moveRight : Cell -> Cell
moveRight c =
    if c.x >= mapSize then
        { c | x = 1 }

    else
        { c | x = c.x + 1 }


moveCell : Direction -> Cell -> Cell
moveCell direction c =
    case direction of
        Up ->
            c |> moveUp

        Right ->
            c |> moveRight

        Down ->
            c |> moveDown

        Left ->
            c |> moveLeft


moveSnake : Model -> Model
moveSnake model =
    let
        newHead =
            List.head model.snake
                |> Maybe.withDefault { x = 1, y = 1 }
                |> moveCell model.direction

        newBody =
            List.take (List.length model.snake - 1) model.snake

        ( newSnake, newStatus ) =
            if List.all (\c -> sameCell c newHead /= True) newBody then
                ( newHead :: newBody, OnGoing )

            else
                ( model.snake, GameOver )
    in
    { model
        | snake = newSnake
        , status = newStatus
    }


play : Model -> ( Model, Cmd Msg )
play model =
    case model.status of
        OnGoing ->
            ( model |> moveSnake, Cmd.none )

        GameOver ->
            ( model, Cmd.none )


oppositeDirections : Direction -> Direction -> Bool
oppositeDirections d1 d2 =
    (d1 == Up && d2 == Down)
        || (d1 == Down && d2 == Up)
        || (d1 == Left && d2 == Right)
        || (d1 == Right && d2 == Left)


changeDirection : Model -> Direction -> ( Model, Cmd Msg )
changeDirection model direction =
    if oppositeDirections direction model.direction then
        ( model, Cmd.none )

    else
        ( { model | direction = direction }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        TimeTick _ ->
            play model

        ChangeDirection direction ->
            changeDirection model direction

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
