module Main exposing (init, main, update)

import Browser
import Keyboard exposing (KeyboardConfig, subscription)
import Model exposing (Cell, Direction(..), Model, Msg(..), Status(..), mapSize, sameCell)
import Movement exposing (moveCell)
import Random
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
      , timePeriod = 0
      , bonuses = []
      }
    , Cmd.none
    )


moveSnake : Model -> ( List Cell, Status )
moveSnake model =
    let
        newHead =
            List.head model.snake
                |> Maybe.withDefault { x = 1, y = 1 }
                |> moveCell model.direction

        newBody =
            List.take (List.length model.snake - 1) model.snake
    in
    if List.all (\c -> sameCell c newHead /= True) newBody then
        ( newHead :: newBody, OnGoing )

    else
        ( model.snake, GameOver )


cellGenerator : Random.Generator ( Int, Int )
cellGenerator =
    Random.pair (Random.int 1 mapSize) (Random.int 1 mapSize)


generateNewBonuses : Model -> ( Cmd Msg, Int )
generateNewBonuses model =
    if model.timePeriod == 20 then
        ( Random.generate NewBonus cellGenerator, 0 )

    else
        ( Cmd.none, model.timePeriod + 1 )


play : Model -> ( Model, Cmd Msg )
play model =
    case model.status of
        OnGoing ->
            let
                ( newSnake, newStatus ) =
                    moveSnake model

                ( cmd, newTimePeriod ) =
                    generateNewBonuses model
            in
            ( { model
                | snake = newSnake
                , status = newStatus
                , timePeriod = newTimePeriod
              }
            , cmd
            )

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

        NewBonus ( x, y ) ->
            let
                newBonuses =
                    { x = x, y = y } :: model.bonuses

                remainingBonuses =
                    if List.length newBonuses > 3 then
                        List.take 3 newBonuses

                    else
                        newBonuses
            in
            ( { model | bonuses = remainingBonuses }, Cmd.none )

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
