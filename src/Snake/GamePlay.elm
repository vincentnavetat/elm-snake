module Snake.GamePlay exposing (play, spawnNewItem)

import List.Extra exposing (remove)
import Random
import Snake.Cell exposing (Cell, sameCell)
import Snake.Model exposing (ItemType(..), Model, Msg(..), Status(..))
import Snake.Movement exposing (moveCell)
import Snake.Snake exposing (snakeIsAlive)


spawnNewItem : Model -> Cell -> ( Model, Cmd Msg )
spawnNewItem model cell =
    let
        config =
            model.config

        newItem =
            { cell = cell, type_ = Bonus }

        itemIsNotOnSnake =
            List.all (\c -> sameCell c newItem.cell /= True) model.snake
    in
    if itemIsNotOnSnake then
        ( { model | items = newItem :: model.items |> List.take config.maxNumberOfItems }, Cmd.none )

    else
        ( model, newItemCmd model )


snakeEatsItem : Cell -> Model -> Model
snakeEatsItem newHead model =
    let
        config =
            model.config

        eatenItem =
            List.Extra.find (\c -> sameCell c.cell newHead) model.items

        newItems =
            case eatenItem of
                Just b ->
                    List.Extra.remove b model.items

                Nothing ->
                    model.items
    in
    if List.any (\c -> sameCell c.cell newHead) model.items then
        { model
            | snake = newHead :: model.snake
            , items = newItems
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


newItemCmd : Model -> Cmd Msg
newItemCmd model =
    let
        config =
            model.config
    in
    Random.generate NewItem (cellGenerator config.mapSize)


generateNewItems : Model -> Cmd Msg
generateNewItems model =
    if modBy 20 model.timeLapses == 0 then
        newItemCmd model

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
                |> snakeEatsItem newHead
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
            , generateNewItems model
            )

        GameOver ->
            ( model, Cmd.none )
