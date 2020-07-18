module Snake.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Snake.Direction exposing (Direction(..))
import Snake.Model exposing (Cell, Model, Msg(..), Status(..), mapSize, sameCell)


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

        isHead =
            sameCell cell (model.snake |> List.head |> Maybe.withDefault { x = 1, y = 1 })

        isBonus =
            List.member cell model.bonuses
    in
    div
        [ class "cell"
        , classList
            [ ( "cell--snake", isSnake )
            , ( "cell--snake-head", isHead )
            , ( "cell--bonus", isBonus )
            ]
        ]
        []


viewGameStatus : Status -> Html Msg
viewGameStatus s =
    if s == GameOver then
        text "Game over looooser!"

    else
        text ""


viewScore : Int -> Html Msg
viewScore s =
    s |> String.fromInt |> text


view : Model -> Html Msg
view model =
    node "main"
        []
        [ div [ class "map" ]
            (drawCols model)
        , div []
            [ viewGameStatus model.status
            , viewScore model.score
            ]
        ]
