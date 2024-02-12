module Functions.Shapes.SShape exposing (..)

import Constants.PlayFieldSizes exposing (evenRowColumnCells, unevenRowColumnCells)
import Functions.Base exposing (isEven)
import Functions.BrickForm exposing (BrickForm(..), TwoFormType(..), switchTwoFormType)
import Functions.PlayFieldDictKeys exposing (makePlayFieldDictKey)
import Functions.Shapes.Comparisons exposing (isEvenIsLeftAndBaseColumnIs, isEvenIsRightAndBaseColumnIs, isUnEvenIsRightAndBaseColumnIs)
import Models exposing (BrickModel)


switchSShapeBrickForm : TwoFormType -> BrickModel -> BrickModel
switchSShapeBrickForm formType brick =
    let
        newFormType =
            switchTwoFormType formType

        newBrickDictKeys =
            createSShapePlayFieldDictKeys brick.baseRow brick.baseColumn newFormType
    in
    { brick | form = SShape newFormType, playFieldDictKeys = newBrickDictKeys }


createSShapePlayFieldDictKeys : Int -> Int -> TwoFormType -> List String
createSShapePlayFieldDictKeys startRowNumber startColumnNumber formType =
    let
        baseKey =
            makePlayFieldDictKey startRowNumber startColumnNumber
    in
    case formType of
        D ->
            createDSShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        E ->
            createESShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey


createDSShapePlayFieldDictKeys : Int -> Int -> String -> List String
createDSShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowRightColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) upRowRightColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber + 1) upRowRightColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 2) startColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createESShapePlayFieldDictKeys : Int -> Int -> String -> List String
createESShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        downRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        downRowNumber =
            startRowNumber + 1

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        thirdKey =
            makePlayFieldDictKey downRowNumber downRowLeftColumnNumber

        fourthKey =
            makePlayFieldDictKey downRowNumber (downRowLeftColumnNumber - 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


doesSShapeBumpWallWhenDropped : TwoFormType -> BrickModel -> Bool
doesSShapeBumpWallWhenDropped formType brick =
    case formType of
        D ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else
                False

        E ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 1) brick then
                True

            else
                False
