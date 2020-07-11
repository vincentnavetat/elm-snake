module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (Cell, Direction(..), Model, Msg(..), Status(..), mapSize)


drawCols : Model -> List (Html Msg)
drawCols model =
    List.range 1 mapSize
        |> List.map (drawRow model)


drawRow : Model -> Int -> Html Msg
drawRow model y =
    let
        row =
            List.range 1 mapSize
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
        [-- cell.x |> String.fromInt |> text
         -- , text " - "
         -- , cell.y |> String.fromInt |> text
        ]


viewGameStatus : Status -> Html Msg
viewGameStatus s =
    if s == GameOver then
        text "Game over looooser!"

    else
        text ""


view : Model -> Html Msg
view model =
    node "main"
        []
        [ div [ class "map" ]
            (drawCols model)
        , div []
            [ viewGameStatus model.status
            ]
        ]
