module Movement exposing (moveCell)

import Model exposing (Cell, Direction(..), mapSize)


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
