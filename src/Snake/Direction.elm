module Snake.Direction exposing (Direction(..), changeDirection)


type Direction
    = Up
    | Right
    | Down
    | Left


oppositeDirections : Direction -> Direction -> Bool
oppositeDirections d1 d2 =
    (d1 == Up && d2 == Down)
        || (d1 == Down && d2 == Up)
        || (d1 == Left && d2 == Right)
        || (d1 == Right && d2 == Left)


changeDirection : Direction -> Direction -> Direction
changeDirection newDirection oldDirection =
    if oppositeDirections oldDirection newDirection then
        oldDirection

    else
        newDirection
