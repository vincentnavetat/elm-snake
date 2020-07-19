module Snake.Model exposing (Item, ItemType(..), Model, Msg(..), Status(..))

import Snake.Cell exposing (Cell)
import Snake.Direction exposing (Direction(..))
import Time


type Status
    = OnGoing
    | GameOver


type ItemType
    = Bonus
    | Penalty


type alias Item =
    { cell : Cell
    , type_ : ItemType
    }


type alias Model =
    { config : Config
    , snake : List Cell
    , direction : Direction
    , status : Status
    , timeLapses : Int
    , items : List Item
    , score : Int
    }


type alias Config =
    { mapSize : Int
    , speed : Float
    , maxNumberOfItems : Int
    }


type Msg
    = TimeTick Time.Posix
    | ChangeDirection Direction
    | NewItem ( Int, Int )
    | Restart
    | NoOp
