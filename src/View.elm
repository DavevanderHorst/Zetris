module View exposing (..)

import Dict exposing (Dict)
import Functions.Base exposing (isEven)
import Functions.Playfield exposing (getRowAndColNumberFromPlayFieldDictKey)
import Html exposing (Html, button, div, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Models exposing (Cell, Color(..), Model)
import PlayFieldSizes
    exposing
        ( cellSizeInPxString
        , halfCellWidthInPxString
        , negativeUpPaddingInPxString
        , playFieldBorderPaddingInPxString
        , playFieldHeightInPxString
        , playFieldWidthInPxString
        )


view : Model -> Html Msg
view model =
    div
        [ attrFloat (Attr.style "width") model.windowSize.width
        , attrFloat (Attr.style "height") model.windowSize.height
        , Attr.style "display" "flex"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        ]
        [ button [ Attr.style "margin-right" "50px", onClick StartGame ] [ text "Start game" ]
        , div
            [ Attr.style "width" playFieldWidthInPxString
            , Attr.style "height" playFieldHeightInPxString
            , Attr.style "background-color" "black"
            ]
            [ renderPlayField model ]
        , case model.error of
            Nothing ->
                div [] []

            Just error ->
                div [] [ text error ]
        ]


renderPlayField : Model -> Html Msg
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
            Dict.foldl renderCell [] row

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
            getCellColor cell.color

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


attrFloat : (String -> Html.Attribute msg) -> Float -> Html.Attribute msg
attrFloat attr value =
    attr (String.fromFloat value ++ "px")


makeViewPlayField : Model -> Dict Int (Dict Int Cell)
makeViewPlayField model =
    let
        fieldToUse =
            Maybe.withDefault model.playField model.tempPlayField
    in
    Dict.foldl insertCell Dict.empty fieldToUse


insertCell : String -> Cell -> Dict Int (Dict Int Cell) -> Dict Int (Dict Int Cell)
insertCell key value dict =
    let
        ( rowNumber, colNumber ) =
            getRowAndColNumberFromPlayFieldDictKey key

        maybeRowDict =
            Dict.get rowNumber dict

        colDict =
            case maybeRowDict of
                Nothing ->
                    Dict.insert colNumber value Dict.empty

                Just rowDict ->
                    Dict.insert colNumber value rowDict
    in
    Dict.insert rowNumber colDict dict


getCellColor : Color -> String
getCellColor color =
    case color of
        White ->
            "white"

        Red ->
            "red"
