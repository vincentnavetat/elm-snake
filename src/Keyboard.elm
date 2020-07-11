module Keyboard exposing (KeyboardConfig, subscription)

import Browser.Events exposing (onKeyDown)
import Json.Decode as Decode


type alias KeyboardConfig msg =
    { up : msg
    , right : msg
    , down : msg
    , left : msg
    , noop : msg
    }


subscription : KeyboardConfig msg -> Sub msg
subscription config =
    onKeyDown (keyDecoder config)


keyDecoder : KeyboardConfig msg -> Decode.Decoder msg
keyDecoder config =
    Decode.map (keyToMsg config) (Decode.field "key" Decode.string)


keyToMsg : KeyboardConfig msg -> String -> msg
keyToMsg config string =
    case string of
        "ArrowUp" ->
            config.up

        "ArrowRight" ->
            config.right

        "ArrowDown" ->
            config.down

        "ArrowLeft" ->
            config.left

        _ ->
            config.noop
