module Functions.Playfield exposing (..)

import Constants.PlayFieldSizes exposing (evenRowColumnCells, evenRowMiddleColumnCellNumber, unevenRowColumnCells)
import Dict exposing (Dict)
import Functions.Base exposing (isEven)
import Functions.Brick exposing (getCurrentRowsFromBrick)
import Functions.Colors exposing (cellColorToString, getBrickFormColor)
import Functions.PlayFieldDictKeys exposing (makePlayFieldDictKey)
import Models exposing (BrickModel, Cell, Color(..), GameModel, MainModel)


getCellFromPlayField : String -> Dict String Cell -> Maybe Cell
getCellFromPlayField key playField =
    Dict.get key playField


changeRowColorInPlayField : Color -> Int -> Dict String Cell -> Dict String Cell
changeRowColorInPlayField color rowNumber playField =
    let
        rowKeys =
            generateRowKeys rowNumber
    in
    List.foldl (setCellInPlayField color) playField rowKeys


generateRowKeys : Int -> List String
generateRowKeys rowNumber =
    let
        max =
            if isEven rowNumber then
                evenRowColumnCells

            else
                unevenRowColumnCells

        colNumbers =
            List.range 1 max
    in
    List.foldl (generateRowKey rowNumber) [] colNumbers


generateRowKey : Int -> Int -> List String -> List String
generateRowKey row col keys =
    let
        key =
            makePlayFieldDictKey row col
    in
    key :: keys


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
                    ( False, "Cell is not empty : " ++ cellColorToString cell.color )

    else
        result


checkForZetris : Dict String Cell -> List Int -> List Int
checkForZetris playField removedRows =
    -- removedRows will never be empty
    let
        maxRow =
            Maybe.withDefault 0 (List.maximum removedRows)

        rowsListToCheck =
            List.range 1 maxRow
    in
    List.foldl (addRowNumberWhenFull playField) [] rowsListToCheck


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


dropRowNumberInPlayField : Int -> Int -> Dict String Cell -> Result String (Dict String Cell)
dropRowNumberInPlayField rowNumber numberOfRowsToDrop playField =
    let
        ( maxColumnNumber, isEvenRowNumber ) =
            if isEven rowNumber then
                ( evenRowColumnCells, True )

            else
                ( unevenRowColumnCells, False )
    in
    if isEven numberOfRowsToDrop then
        -- row keeps same size, so easiest drop.
        let
            columnNumberList =
                List.range 1 maxColumnNumber
        in
        dropColumnNumbersInPlayFieldNormalFunc rowNumber numberOfRowsToDrop columnNumberList playField

    else
        -- the first half of the list always stays same number, so we can drop those already.
        let
            firstHalfColumnNumberList =
                List.range 1 (evenRowMiddleColumnCellNumber - 1)

            firstHalfPlayFieldResult =
                dropColumnNumbersInPlayFieldNormalFunc rowNumber numberOfRowsToDrop firstHalfColumnNumberList playField
        in
        case firstHalfPlayFieldResult of
            Err err ->
                Err err

            Ok firstHalfPlayField ->
                -- need to check if we go from even to uneven or even to uneven
                if isEvenRowNumber then
                    -- we go from even to uneven, so second half needs + 1
                    -- column 8 always goes right
                    -- next uneven row middle cell, we need to set to white
                    let
                        secondHalfColumnNumberList =
                            List.range evenRowMiddleColumnCellNumber evenRowColumnCells

                        secondHalfPlayFieldResult =
                            dropColumnNumbersInPlayFieldOneHigherFunc rowNumber numberOfRowsToDrop secondHalfColumnNumberList firstHalfPlayField
                    in
                    case secondHalfPlayFieldResult of
                        Err err ->
                            Err err

                        Ok secondHalfPlayField ->
                            let
                                keyToClear =
                                    makePlayFieldDictKey (rowNumber + numberOfRowsToDrop) 8
                            in
                            Ok (setCellInPlayField White keyToClear secondHalfPlayField)

                else
                    -- we go from uneven to even, so second half needs - 1
                    -- column 8 we do after
                    let
                        secondHalfColumnNumberList =
                            List.range (evenRowMiddleColumnCellNumber + 1) unevenRowColumnCells

                        secondHalfPlayFieldResult =
                            dropColumnNumbersInPlayFieldOneLowerFunc rowNumber numberOfRowsToDrop secondHalfColumnNumberList firstHalfPlayField
                    in
                    case secondHalfPlayFieldResult of
                        Err err ->
                            Err err

                        Ok secondHalfPlayField ->
                            dropColumnEightForUnevenToEvenRowDrop rowNumber numberOfRowsToDrop secondHalfPlayField


dropColumnEightForUnevenToEvenRowDrop : Int -> Int -> Dict String Cell -> Result String (Dict String Cell)
dropColumnEightForUnevenToEvenRowDrop rowNumber numberOfRowsToDrop playField =
    let
        currentKey =
            makePlayFieldDictKey rowNumber 8

        currentCellResult =
            getCellFromPlayField currentKey playField
    in
    case currentCellResult of
        Nothing ->
            Err ("No cell found for column eight : " ++ currentKey)

        Just currentCell ->
            if currentCell.color == White then
                Ok playField

            else
                setColumnEightRecursive (rowNumber + numberOfRowsToDrop) 8 0 Up currentCell.color playField


setColumnEightRecursive : Int -> Int -> Int -> ColumnEightDropDirection -> Color -> Dict String Cell -> Result String (Dict String Cell)
setColumnEightRecursive rowNumber colNumber shift shiftDirection color playField =
    let
        nextShift =
            shift + 1

        ( nextColNumber, nextShiftDirection ) =
            case shiftDirection of
                Up ->
                    ( colNumber + shift, Down )

                Down ->
                    ( colNumber - shift, Up )

        nextKey =
            makePlayFieldDictKey rowNumber nextColNumber

        currentCellResult =
            getCellFromPlayField nextKey playField
    in
    case currentCellResult of
        Just cell ->
            if cell.color == White then
                Ok (setCellInPlayField color nextKey playField)

            else
                setColumnEightRecursive rowNumber nextColNumber nextShift nextShiftDirection color playField

        Nothing ->
            Err ("Cant set column eight, didnt find a cell for " ++ nextKey)


dropColumnNumbersInPlayFieldNormalFunc : Int -> Int -> List Int -> Dict String Cell -> Result String (Dict String Cell)
dropColumnNumbersInPlayFieldNormalFunc rowNumber numberOfRowsToDrop columnNumbers playField =
    dropColumnNumbersInPlayFieldFunc rowNumber numberOfRowsToDrop columnNumbers Normal playField


dropColumnNumbersInPlayFieldOneHigherFunc : Int -> Int -> List Int -> Dict String Cell -> Result String (Dict String Cell)
dropColumnNumbersInPlayFieldOneHigherFunc rowNumber numberOfRowsToDrop columnNumbers playField =
    dropColumnNumbersInPlayFieldFunc rowNumber numberOfRowsToDrop columnNumbers OneHigher playField


dropColumnNumbersInPlayFieldOneLowerFunc : Int -> Int -> List Int -> Dict String Cell -> Result String (Dict String Cell)
dropColumnNumbersInPlayFieldOneLowerFunc rowNumber numberOfRowsToDrop columnNumbers playField =
    dropColumnNumbersInPlayFieldFunc rowNumber numberOfRowsToDrop columnNumbers OneLower playField


dropColumnNumbersInPlayFieldFunc : Int -> Int -> List Int -> ColumnDrop -> Dict String Cell -> Result String (Dict String Cell)
dropColumnNumbersInPlayFieldFunc rowNumber numberOfRowsToDrop columnNumbers dropType playField =
    List.foldl (dropColumnNumberInPlayField rowNumber numberOfRowsToDrop dropType) (Ok playField) columnNumbers


dropColumnNumberInPlayField : Int -> Int -> ColumnDrop -> Int -> Result String (Dict String Cell) -> Result String (Dict String Cell)
dropColumnNumberInPlayField rowNumber numberOfRowsToDrop dropType columnNumber playFieldResult =
    case playFieldResult of
        Err err ->
            Err err

        Ok playField ->
            let
                currentKey =
                    makePlayFieldDictKey rowNumber columnNumber

                currentCellResult =
                    getCellFromPlayField currentKey playField
            in
            case currentCellResult of
                Nothing ->
                    Err ("No cell found for key : " ++ currentKey)

                Just currentCell ->
                    let
                        newRowNumber =
                            rowNumber + numberOfRowsToDrop

                        newColumnNumber =
                            case dropType of
                                Normal ->
                                    columnNumber

                                OneHigher ->
                                    columnNumber + 1

                                OneLower ->
                                    columnNumber - 1

                        newKey =
                            makePlayFieldDictKey newRowNumber newColumnNumber
                    in
                    Ok (setCellInPlayField currentCell.color newKey playField)


type ColumnDrop
    = Normal
    | OneHigher
    | OneLower


type ColumnEightDropDirection
    = Up
    | Down
