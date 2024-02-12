module Views.PlayFieldView exposing (..)

import Constants.PlayFieldSizes exposing (cellSizeInPxString, halfCellWidthInPxString, negativeUpPaddingInPxString, playFieldBorderPaddingInPxString)
import Dict exposing (Dict)
import Functions.Base exposing (isEven)
import Functions.Colors exposing (cellColorToString)
import Html exposing (Html, div)
import Html.Attributes as Attr
import Messages exposing (Msg)
import Models exposing (Cell, MainModel)
import Views.ViewHelpers exposing (makeViewPlayField)


renderPlayField : MainModel -> Html Msg
renderPlayField model =
    let
        playFieldForView =
            makeViewPlayField model
    in
    div [ Attr.style "padding" playFieldBorderPaddingInPxString ] (Dict.foldr renderPlayFieldRows [] playFieldForView)


renderPlayFieldRows : Int -> Dict Int Cell -> List (Html Msg) -> List (Html Msg)
renderPlayFieldRows rowNumber row list =
    let
        newRow =
            renderPlayFieldRow rowNumber row
    in
    newRow :: list


renderPlayFieldRow : Int -> Dict Int Cell -> Html Msg
renderPlayFieldRow rowNumber row =
    let
        newRow =
            Dict.foldr renderCell [] row

        finishedRow =
            if isEven rowNumber then
                evenSpaceDiv :: newRow

            else
                newRow
    in
    div (rowAttributes rowNumber) finishedRow


gridPolygon : String
gridPolygon =
    "polygon(0% 25%, 0% 75%, 50% 100%, 100% 75%, 100% 25%, 50% 0%)"


rowAttributes : Int -> List (Html.Attribute msg)
rowAttributes rowNumber =
    [ Attr.style "display" "flex"
    , Attr.style "position" "relative"
    , Attr.style "height" cellSizeInPxString
    , Attr.style "top" (negativeUpPaddingInPxString rowNumber)
    ]


evenSpaceDiv : Html Msg
evenSpaceDiv =
    div [ Attr.style "width" halfCellWidthInPxString ] []


renderCell : key -> Cell -> List (Html Msg) -> List (Html Msg)
renderCell _ cell htmlList =
    let
        cellColor =
            cellColorToString cell.color

        newCell =
            div
                [ Attr.style "background-color" cellColor
                , Attr.style "clip-path" gridPolygon
                , Attr.style "width" cellSizeInPxString
                , Attr.style "height" "100%"
                , Attr.style "margin-right" "1px"
                ]
                []
    in
    newCell :: htmlList
