module Model exposing (Cell, Direction(..), Model, Msg(..), Status(..), mapSize)

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


type alias Model =
    { snake : List Cell
    , direction : Direction
    , status : Status
    }


type Msg
    = TimeTick Time.Posix
    | ChangeDirection Direction
    | NoOp
