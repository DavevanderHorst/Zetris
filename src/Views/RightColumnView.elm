module Views.RightColumnView exposing (..)

import Functions.BrickMoveDirection exposing (brickMoveDirectionToString)
import Html exposing (Html, button, div, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Models exposing (MainModel)
import Views.ViewHelpers exposing (attrInt)


rightColumnView : MainModel -> Html Msg
rightColumnView model =
    div []
        [ buttonsView
        , scoreView model.gameModel.score
        , errorView model
        ]


buttonsView =
    div
        [ Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        , Attr.style "width" "100%"
        , Attr.style "height" "100px"
        , Attr.style "border" "2px solid black"
        , Attr.style "margin-bottom" "20px"
        ]
        [ button
            [ onClick StartGame
            ]
            [ text "Start game" ]
        ]


scoreView : Int -> Html Msg
scoreView score =
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        , Attr.style "width" "100%"
        , Attr.style "height" "300px"
        , Attr.style "border" "2px solid black"
        ]
        [ div
            [ Attr.style "margin-bottom" "10px"
            , Attr.style "font-size" "25px"
            ]
            [ text "SCORE :" ]
        , div [ Attr.style "font-size" "40px" ] [ text (String.fromInt score) ]
        ]


errorView : MainModel -> Html Msg
errorView model =
    case model.gameModel.currentBrickModel of
        Nothing ->
            div [] [ text "No current brick model." ]

        Just brickModel ->
            div []
                [ case model.error of
                    Nothing ->
                        div [] []

                    Just error ->
                        div [] [ text error ]
                , div [] [ text ("direction : " ++ brickMoveDirectionToString brickModel.direction) ]
                , div [] [ text ("row : " ++ String.fromInt brickModel.baseRow) ]
                , div [] [ text ("column : " ++ String.fromInt brickModel.baseColumn) ]
                , div [] [ text ("column : " ++ String.fromInt brickModel.baseColumn) ]
                ]
