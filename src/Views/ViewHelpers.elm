module Views.ViewHelpers exposing (..)

import Dict exposing (Dict)
import Functions.PlayFieldDictKeys exposing (getRowAndColNumberFromPlayFieldDictKey)
import Html
import Models exposing (Cell, MainModel)


attrFloat : (String -> Html.Attribute msg) -> Float -> Html.Attribute msg
attrFloat attr value =
    attr (String.fromFloat value ++ "px")


attrInt : (String -> Html.Attribute msg) -> Int -> Html.Attribute msg
attrInt attr value =
    attr (String.fromInt value ++ "px")


makeViewPlayField : MainModel -> Dict Int (Dict Int Cell)
makeViewPlayField model =
    let
        fieldToUse =
            Maybe.withDefault model.gameModel.playField model.gameModel.tempPlayField
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
