module Snake.Cell exposing (Cell, sameCell)


type alias Cell =
    { x : Int
    , y : Int
    }


sameCell : Cell -> Cell -> Bool
sameCell c1 c2 =
    c1.x == c2.x && c1.y == c2.y
