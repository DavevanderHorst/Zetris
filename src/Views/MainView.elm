module Views.MainView exposing (..)

import Functions.BrickMoveDirection exposing (brickMoveDirectionToString)
import Html exposing (Html, button, div, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Models exposing (BrickModel, Cell, Color(..), MainModel)
import PlayFieldSizes exposing (playFieldHeight, playFieldWidth)
import Views.ErrorScreen exposing (errorScreen)
import Views.PlayFieldView exposing (renderPlayField)
import Views.ViewHelpers exposing (attrFloat, attrInt)


mainView : MainModel -> Html Msg
mainView model =
    let
        width =
            model.windowSize.width

        height =
            model.windowSize.height

        nonPlayFieldWidth =
            width - toFloat playFieldWidth

        nonPlayFieldColumnWidth =
            nonPlayFieldWidth / 2

        minWindowWidth =
            playFieldWidth + leftColumnMinWidth + rightColumnMinWidth

        minWindowHeight =
            playFieldHeight
    in
    if width < toFloat minWindowWidth || height < toFloat minWindowHeight then
        errorScreen

    else
        div
            [ attrFloat (Attr.style "width") width
            , attrFloat (Attr.style "height") height
            , Attr.style "display" "flex"
            ]
            [ -- left column
              div
                [ attrFloat (Attr.style "width") nonPlayFieldColumnWidth
                , Attr.style "display" "flex"
                , Attr.style "justify-content" "right"
                , Attr.style "align-items" "center"
                ]
                [ button
                    [ onClick StartGame
                    , Attr.style "margin-right" "20px"
                    ]
                    [ text "Start game" ]
                ]
            , -- playing field
              div
                [ attrInt (Attr.style "width") playFieldWidth
                , Attr.style "display" "flex"
                , Attr.style "align-items" "center"
                ]
                [ div
                    [ Attr.style "width" "100%"
                    , attrInt (Attr.style "height") playFieldHeight
                    , Attr.style "background-color" "black"
                    ]
                    [ renderPlayField model ]
                ]
            , -- right column
              div
                [ attrFloat (Attr.style "width") nonPlayFieldColumnWidth
                , Attr.style "margin" "20px"
                ]
                [ case model.gameModel.currentBrickModel of
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
                ]
            ]


leftColumnMinWidth : Int
leftColumnMinWidth =
    100


rightColumnMinWidth : Int
rightColumnMinWidth =
    100
