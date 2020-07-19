module Snake.GamePlay exposing (play, spawnNewBonus)

import List.Extra exposing (remove)
import Random
import Snake.Cell exposing (Cell, sameCell)
import Snake.Model exposing (Model, Msg(..), Status(..))
import Snake.Movement exposing (moveCell)
import Snake.Snake exposing (snakeIsAlive)


spawnNewBonus : Model -> Cell -> ( Model, Cmd Msg )
spawnNewBonus model bonus =
    let
        config =
            model.config

        bonusIsNotOnSnake =
            List.all (\c -> sameCell c bonus /= True) model.snake
    in
    if bonusIsNotOnSnake then
        ( { model | bonuses = bonus :: model.bonuses |> List.take config.maxNumberOfBonuses }, Cmd.none )

    else
        ( model, newBonusCmd model )


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


newBonusCmd : Model -> Cmd Msg
newBonusCmd model =
    let
        config =
            model.config
    in
    Random.generate NewBonus (cellGenerator config.mapSize)


generateNewBonuses : Model -> Cmd Msg
generateNewBonuses model =
    if modBy 20 model.timeLapses == 0 then
        newBonusCmd model

    else
        Cmd.none


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
            in
            ( { newModel
                | timeLapses = model.timeLapses + 1
              }
            , generateNewBonuses model
            )

        GameOver ->
            ( model, Cmd.none )
