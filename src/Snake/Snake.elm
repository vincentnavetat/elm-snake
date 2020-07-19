module Snake.Snake exposing (initSnake, snakeHead, snakeIsAlive)

import Snake.Cell exposing (Cell, sameCell)


initSnake : Int -> List Cell
initSnake size =
    List.range 1 size
        |> List.map (\x -> { x = x, y = 1 })
        |> List.reverse


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
