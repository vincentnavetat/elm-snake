module Model exposing (Cell, Direction(..), Model, Msg(..), Status(..), mapSize, sameCell)

import Time


mapSize : Int
mapSize =
    20


type Status
    = OnGoing
    | GameOver


type Direction
    = Up
    | Right
    | Down
    | Left


type alias Cell =
    { x : Int
    , y : Int
    }


sameCell : Cell -> Cell -> Bool
sameCell c1 c2 =
    c1.x == c2.x && c1.y == c2.y


type alias Model =
    { snake : List Cell
    , direction : Direction
    , status : Status
    }


type Msg
    = TimeTick Time.Posix
    | ChangeDirection Direction
    | NoOp
