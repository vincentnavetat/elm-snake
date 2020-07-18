module Snake.Direction exposing (Direction(..), oppositeDirections)


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
