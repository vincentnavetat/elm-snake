module Snake.GamePlay exposing (play, spawnNewItem)

import List.Extra exposing (find, remove)
import Random
import Snake.Cell exposing (Cell, sameCell)
import Snake.Model exposing (Item, ItemType(..), Model, Msg(..), Status(..))
import Snake.Movement exposing (moveCell)
import Snake.Snake exposing (snakeIsAlive)


spawnNewItem : Model -> Cell -> ( Model, Cmd Msg )
spawnNewItem model cell =
    let
        config =
            model.config

        newType =
            if modBy 3 cell.x == 0 then
                Penalty

            else
                Bonus

        newItem =
            { cell = cell, type_ = newType }

        itemIsNotOnSnake =
            List.all (\c -> sameCell c newItem.cell /= True) model.snake
    in
    if itemIsNotOnSnake then
        ( { model | items = newItem :: model.items |> List.take config.maxNumberOfItems }, Cmd.none )

    else
        ( model, newItemCmd model )


updateSnake : Cell -> Model -> Model
updateSnake newHead model =
    let
        eatenItem =
            find (\c -> sameCell c.cell newHead) model.items
    in
    case eatenItem of
        Just i ->
            { model | snake = newHead :: model.snake }
                |> snakeEats i

        Nothing ->
            { model
                | snake = newHead :: List.take (List.length model.snake - 1) model.snake
            }


snakeEats : Item -> Model -> Model
snakeEats item model =
    case item.type_ of
        Bonus ->
            { model
                | items = remove item model.items
                , score = model.score + 1
            }

        Penalty ->
            { model
                | items = remove item model.items
                , score = model.score - 3
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


generateNewItem : Model -> Cmd Msg
generateNewItem model =
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
                |> updateSnake newHead
    in
    if snakeIsAlive newModel.snake then
        { newModel
            | status = OnGoing
        }

    else
        { model | status = GameOver }


increaseSpeed : Model -> Model
increaseSpeed model =
    let
        config =
            model.config
    in
    if modBy 50 model.timeLapses == 0 then
        { model | config = { config | speed = config.speed - 10 } }

    else
        model


play : Model -> ( Model, Cmd Msg )
play model =
    case model.status of
        OnGoing ->
            let
                newModel =
                    model
                        |> moveSnake
                        |> increaseSpeed
            in
            ( { newModel
                | timeLapses = model.timeLapses + 1
              }
            , generateNewItem model
            )

        GameOver ->
            ( model, Cmd.none )
