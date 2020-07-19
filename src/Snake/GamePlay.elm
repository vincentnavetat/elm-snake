module Snake.GamePlay exposing (play, spawnNewBonus)

import List.Extra exposing (remove)
import Random
import Snake.Cell exposing (Cell, sameCell)
import Snake.Model exposing (Model, Msg(..), Status(..))
import Snake.Movement exposing (moveCell)


spawnNewBonus : Model -> Cell -> ( Model, Cmd Msg )
spawnNewBonus model c =
    let
        config =
            model.config
    in
    ( { model | bonuses = c :: model.bonuses |> List.take config.maxNumberOfBonuses }, Cmd.none )


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


snakeEatsBonus : Cell -> Model -> Model
snakeEatsBonus newHead model =
    let
        config =
            model.config
    in
    if List.any (\c -> sameCell c newHead) model.bonuses then
        { model
            | snake = newHead :: model.snake
            , bonuses = List.Extra.remove newHead model.bonuses
            , score = model.score + 1
            , config = { config | speed = config.speed - 10 }
        }

    else
        { model
            | snake = newHead :: List.take (List.length model.snake - 1) model.snake
        }


cellGenerator : Int -> Random.Generator ( Int, Int )
cellGenerator mapSize =
    Random.pair (Random.int 1 mapSize) (Random.int 1 mapSize)


generateNewBonuses : Model -> ( Cmd Msg, Int )
generateNewBonuses model =
    let
        config =
            model.config
    in
    if model.timePeriod == 20 then
        ( Random.generate NewBonus (cellGenerator config.mapSize), 0 )

    else
        ( Cmd.none, model.timePeriod + 1 )


moveSnake : Model -> Model
moveSnake model =
    let
        config =
            model.config

        newHead =
            List.head model.snake
                |> Maybe.withDefault { x = 1, y = 1 }
                |> moveCell model.direction config.mapSize

        newModel =
            model
                |> snakeEatsBonus newHead
    in
    if snakeIsAlive newModel.snake then
        { newModel
            | status = OnGoing
        }

    else
        { model | status = GameOver }


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
