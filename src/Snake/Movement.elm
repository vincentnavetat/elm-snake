module Snake.Movement exposing (moveCell)

import Snake.Cell exposing (Cell)
import Snake.Direction exposing (Direction(..))


moveUp : Int -> Cell -> Cell
moveUp mapSize c =
    if c.y <= 1 then
        { c | y = mapSize }

    else
        { c | y = c.y - 1 }


moveDown : Int -> Cell -> Cell
moveDown mapSize c =
    if c.y >= mapSize then
        { c | y = 1 }

    else
        { c | y = c.y + 1 }


moveLeft : Int -> Cell -> Cell
moveLeft mapSize c =
    if c.x <= 1 then
        { c | x = mapSize }

    else
        { c | x = c.x - 1 }


moveRight : Int -> Cell -> Cell
moveRight mapSize c =
    if c.x >= mapSize then
        { c | x = 1 }

    else
        { c | x = c.x + 1 }


moveCell : Direction -> Int -> Cell -> Cell
moveCell direction mapSize c =
    case direction of
        Up ->
            c |> moveUp mapSize

        Right ->
            c |> moveRight mapSize

        Down ->
            c |> moveDown mapSize

        Left ->
            c |> moveLeft mapSize
