module Snake.Main exposing (init, main, update)

import Browser
import List.Extra exposing (remove)
import Random
import Snake.Direction exposing (Direction(..), oppositeDirections)
import Snake.Keyboard exposing (KeyboardConfig, subscription)
import Snake.Model exposing (Cell, Model, Msg(..), Status(..), mapSize, sameCell)
import Snake.Movement exposing (moveCell)
import Snake.View exposing (view)
import Time


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
      , score = 0
      }
    , Cmd.none
    )


snakeHead : List Cell -> Cell
snakeHead snake =
    List.head snake
        |> Maybe.withDefault { x = 1, y = 1 }


snakeBody : List Cell -> List Cell
snakeBody snake =
    snake
        |> List.reverse
        |> List.take (List.length snake - 1)
        |> List.reverse


snakeIsAlive : List Cell -> Bool
snakeIsAlive snake =
    let
        head =
            snakeHead snake

        body =
            snakeBody snake
    in
    List.all (\c -> sameCell c head /= True) body


moveSnake : Model -> Model
moveSnake model =
    let
        newHead =
            List.head model.snake
                |> Maybe.withDefault { x = 1, y = 1 }
                |> moveCell model.direction

        newModel =
            if List.any (\c -> sameCell c newHead) model.bonuses then
                { model
                    | snake = newHead :: model.snake
                    , bonuses = List.Extra.remove newHead model.bonuses
                    , score = model.score + 1
                }

            else
                { model
                    | snake = newHead :: List.take (List.length model.snake - 1) model.snake
                }
    in
    if snakeIsAlive newModel.snake then
        { newModel
            | status = OnGoing
        }

    else
        { model | status = GameOver }


changeDirection : Model -> Direction -> ( Model, Cmd Msg )
changeDirection model direction =
    if oppositeDirections direction model.direction then
        ( model, Cmd.none )

    else
        ( { model | direction = direction }, Cmd.none )


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
                newModel =
                    model |> moveSnake

                ( cmd, newTimePeriod ) =
                    generateNewBonuses model
            in
            ( { newModel
                | timePeriod = newTimePeriod
              }
            , cmd
            )

        GameOver ->
            ( model, Cmd.none )


spawnNewBonus : Model -> Cell -> ( Model, Cmd Msg )
spawnNewBonus model c =
    ( { model | bonuses = c :: model.bonuses |> List.take 3 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        TimeTick _ ->
            play model

        ChangeDirection direction ->
            changeDirection model direction

        NewBonus ( x, y ) ->
            spawnNewBonus model { x = x, y = y }

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
