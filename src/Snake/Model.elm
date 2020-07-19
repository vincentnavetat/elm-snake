module Snake.Model exposing (Model, Msg(..), Status(..))

import Snake.Cell exposing (Cell)
import Snake.Direction exposing (Direction(..))
import Time


type Status
    = OnGoing
    | GameOver


type alias Model =
    { config : Config
    , snake : List Cell
    , direction : Direction
    , status : Status
    , timePeriod : Int
    , bonuses : List Cell
    , score : Int
    }


type alias Config =
    { mapSize : Int
    , speed : Float
    , maxNumberOfBonuses : Int
    }


type Msg
    = TimeTick Time.Posix
    | ChangeDirection Direction
    | NewBonus ( Int, Int )
    | Restart
    | NoOp
