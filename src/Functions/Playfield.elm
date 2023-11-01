module Functions.Playfield exposing (..)

import Dict exposing (Dict)
import Functions.Base exposing (isEven)
import Functions.Brick exposing (getCurrentRowsFromBrick)
import Functions.Colors exposing (cellColorToString, getBrickFormColor)
import Functions.PlayFieldDictKeys exposing (makePlayFieldDictKey)
import Models exposing (BrickModel, Cell, Color(..), GameModel, MainModel)
import PlayFieldSizes exposing (evenRowColumnCells, unevenRowColumnCells)


setBrickInPlayField : BrickModel -> Dict String Cell -> Dict String Cell
setBrickInPlayField brick playField =
    let
        color =
            getBrickFormColor brick.form
    in
    List.foldl (setCellInPlayField color) playField brick.playFieldDictKeys


setCellInPlayField : Color -> String -> Dict String Cell -> Dict String Cell
setCellInPlayField color key playField =
    Dict.insert key (Cell color) playField


canBrickBePlacedInPlayField : List String -> Dict String Cell -> ( Bool, String )
canBrickBePlacedInPlayField keys playField =
    List.foldl (isCellEmpty playField) ( True, "" ) keys


isCellEmpty : Dict String Cell -> String -> ( Bool, String ) -> ( Bool, String )
isCellEmpty dict key result =
    let
        ( isOk, _ ) =
            result
    in
    if isOk then
        let
            cellResult =
                Dict.get key dict
        in
        case cellResult of
            Nothing ->
                ( False, "No cell found for key : " ++ key )

            Just cell ->
                if cell.color == White then
                    result

                else
                    ( False, "Cell has wrong color : " ++ cellColorToString cell.color )

    else
        result


getFullRowsAfterSettingBrick : Dict String Cell -> BrickModel -> List Int
getFullRowsAfterSettingBrick playField brick =
    let
        rowsListToCheck =
            getCurrentRowsFromBrick brick
    in
    List.foldl (addRowNumberWhenFull playField) [] rowsListToCheck


addRowNumberWhenFull : Dict String Cell -> Int -> List Int -> List Int
addRowNumberWhenFull playField row fullRowsList =
    let
        rowKeys =
            createAllKeysForRowNumber row

        isRowFilled =
            List.foldl (checkIfIsFilled playField) True rowKeys
    in
    if isRowFilled then
        row :: fullRowsList

    else
        fullRowsList


checkIfIsFilled : Dict String Cell -> String -> Bool -> Bool
checkIfIsFilled playField key allFilled =
    if allFilled then
        let
            cellResult =
                Dict.get key playField
        in
        case cellResult of
            Nothing ->
                False

            Just cell ->
                if cell.color == White then
                    False

                else
                    True

    else
        False


createAllKeysForRowNumber : Int -> List String
createAllKeysForRowNumber row =
    let
        max =
            if isEven row then
                evenRowColumnCells

            else
                unevenRowColumnCells

        columnNumbers =
            List.range 1 max
    in
    List.foldl (createAndAddDictKey row) [] columnNumbers


createAndAddDictKey : Int -> Int -> List String -> List String
createAndAddDictKey row col keys =
    makePlayFieldDictKey row col :: keys
