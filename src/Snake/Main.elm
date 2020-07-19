module Snake.Main exposing (init, main, update)

import Browser
import Snake.Cell exposing (Cell)
import Snake.Direction exposing (Direction(..), changeDirection)
import Snake.GamePlay exposing (play, spawnNewBonus)
import Snake.Keyboard exposing (KeyboardConfig, subscription)
import Snake.Model exposing (Model, Msg(..), Status(..))
import Snake.View exposing (view)
import Time


initSnake : Int -> List Cell
initSnake size =
    List.range 1 size
        |> List.map (\x -> { x = x, y = 1 })
        |> List.reverse


init : Int -> ( Model, Cmd Msg )
init _ =
    ( { config =
            { mapSize = 20
            , speed = 200
            , maxNumberOfBonuses = 3
            }
      , snake = initSnake 10
      , direction = Right
      , status = OnGoing
      , timePeriod = 0
      , bonuses = []
      , score = 0
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        TimeTick _ ->
            play model

        ChangeDirection direction ->
            ( { model | direction = model.direction |> changeDirection direction }, Cmd.none )

        NewBonus ( x, y ) ->
            spawnNewBonus model { x = x, y = y }

        NoOp ->
            ( model, Cmd.none )


keyboardConfig : KeyboardConfig Msg
keyboardConfig =
    { up = ChangeDirection Up
    , right = ChangeDirection Right
    , down = ChangeDirection Down
    , left = ChangeDirection Left
    , noop = NoOp
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        config =
            model.config
    in
    Sub.batch
        [ subscription keyboardConfig
        , Time.every config.speed TimeTick
        ]


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Snake!"
                , body = [ view m ]
                }
        , subscriptions = subscriptions
        }
