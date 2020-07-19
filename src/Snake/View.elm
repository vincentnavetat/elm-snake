module Snake.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Snake.Cell exposing (Cell, sameCell)
import Snake.Direction exposing (Direction(..))
import Snake.Model exposing (Model, Msg(..), Status(..))
import Snake.Snake exposing (snakeHead)


drawCols : Model -> List (Html Msg)
drawCols model =
    let
        config =
            model.config
    in
    List.range 1 config.mapSize
        |> List.map (drawRow model)


drawRow : Model -> Int -> Html Msg
drawRow model y =
    let
        config =
            model.config

        row =
            List.range 1 config.mapSize
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
            snakeHead model.snake
                |> sameCell cell

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
        div [ class "game-status" ]
            [ div [ class "game-status__content" ]
                [ div []
                    [ text "Game over, "
                    , strong [] [ text "looooser!" ]
                    ]
                , div [ class "game-status__cta" ]
                    [ button [ class "btn", onClick Restart ] [ text "Play again" ]
                    ]
                ]
            ]

    else
        text ""


viewScore : Int -> Html Msg
viewScore s =
    span [ class "game__score" ]
        [ text "Score: "
        , strong []
            [ s |> String.fromInt |> text
            ]
        ]


view : Model -> Html Msg
view model =
    node "main"
        [ class "game" ]
        [ div [ class "map" ]
            (drawCols model ++ [ viewGameStatus model.status ])
        , viewScore model.score
        ]
