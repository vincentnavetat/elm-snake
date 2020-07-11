module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Task
import Time


type alias Cell =
    { x : Int
    , y : Int
    }


type Status
    = OnGoing
    | GameOver


type alias Model =
    { snake : List Cell
    , status : Status
    }


init : Int -> ( Model, Cmd Msg )
init _ =
    ( { snake = [ { x = 1, y = 1 } ]
      , status = OnGoing
      }
    , Cmd.none
    )


type Msg
    = TimeTick Time.Posix


moveSnake : Model -> List Cell
moveSnake model =
    List.map (\c -> { c | x = c.x + 1 }) model.snake


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        TimeTick _ ->
            let
                newSnake =
                    moveSnake model
            in
            ( { model | snake = newSnake }
            , Cmd.none
            )


size : Int
size =
    10


drawCols : Model -> List (Html Msg)
drawCols model =
    List.range 1 size
        |> List.map (drawRow model)


drawRow : Model -> Int -> Html Msg
drawRow model y =
    let
        row =
            List.range 1 size
                |> List.map (\x -> drawCell model { x = x, y = y })
    in
    div [ class "row" ]
        row


drawCell : Model -> Cell -> Html Msg
drawCell model cell =
    let
        isSnake =
            List.member cell model.snake
    in
    div [ class "cell", classList [ ( "cell--snake", isSnake ) ] ]
        [ cell.x |> String.fromInt |> text
        , text " - "
        , cell.y |> String.fromInt |> text
        ]


view : Model -> Html Msg
view model =
    div [ class "map" ]
        (drawCols model)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 TimeTick


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
