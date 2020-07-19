module Snake.Main exposing (init, main, update)

import Browser
import Snake.Direction exposing (Direction(..), changeDirection)
import Snake.GamePlay exposing (play, spawnNewItem)
import Snake.Keyboard exposing (KeyboardConfig, subscription)
import Snake.Model exposing (ItemType(..), Model, Msg(..), Status(..))
import Snake.Snake exposing (initSnake)
import Snake.View exposing (view)
import Time


init : () -> ( Model, Cmd Msg )
init _ =
    ( { config =
            { mapSize = 20
            , speed = 200
            , maxNumberOfItems = 3
            }
      , snake = initSnake 10
      , direction = Right
      , status = OnGoing
      , timeLapses = 0
      , items = []
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

        NewItem ( x, y ) ->
            spawnNewItem model { x = x, y = y }

        Restart ->
            init ()

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


main : Program () Model Msg
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
