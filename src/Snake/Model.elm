module Snake.Model exposing (Cell, Model, Msg(..), Status(..), mapSize, sameCell)

import Snake.Direction exposing (Direction(..))
import Time


mapSize : Int
mapSize =
    20


type Status
    = OnGoing
    | GameOver


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
    , timePeriod : Int
    , bonuses : List Cell
    , score : Int
    }


type Msg
    = TimeTick Time.Posix
    | ChangeDirection Direction
    | NewBonus ( Int, Int )
    | NoOp
