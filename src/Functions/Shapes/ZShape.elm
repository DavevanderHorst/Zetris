module Functions.Shapes.ZShape exposing (..)

import Constants.PlayFieldSizes exposing (evenRowColumnCells, unevenRowColumnCells)
import Functions.Base exposing (isEven)
import Functions.BrickForm exposing (BrickForm(..), TwoFormType(..), switchTwoFormType)
import Functions.PlayFieldDictKeys exposing (makePlayFieldDictKey)
import Functions.Shapes.Comparisons exposing (isEvenIsLeftAndBaseColumnIs, isEvenIsRightAndBaseColumnIs, isUnEvenIsLeftAndBaseColumnIs, isUnEvenIsRightAndBaseColumnIs)
import Models exposing (BrickModel)


switchZShapeBrickForm : TwoFormType -> BrickModel -> BrickModel
switchZShapeBrickForm formType brick =
    let
        newFormType =
            switchTwoFormType formType

        newBrickDictKeys =
            createZShapePlayFieldDictKeys brick.baseRow brick.baseColumn newFormType
    in
    { brick | form = ZShape newFormType, playFieldDictKeys = newBrickDictKeys }


createZShapePlayFieldDictKeys : Int -> Int -> TwoFormType -> List String
createZShapePlayFieldDictKeys startRowNumber startColumnNumber formType =
    let
        baseKey =
            makePlayFieldDictKey startRowNumber startColumnNumber
    in
    case formType of
        D ->
            createDZShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        E ->
            createEZShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey


createDZShapePlayFieldDictKeys : Int -> Int -> String -> List String
createDZShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) upRowLeftColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber + 1) upRowLeftColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 2) startColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createEZShapePlayFieldDictKeys : Int -> Int -> String -> List String
createEZShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        downRowRightColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        downRowNumber =
            startRowNumber + 1

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        thirdKey =
            makePlayFieldDictKey downRowNumber downRowRightColumnNumber

        fourthKey =
            makePlayFieldDictKey downRowNumber (downRowRightColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


doesZShapeBumpWallWhenDropped : TwoFormType -> BrickModel -> Bool
doesZShapeBumpWallWhenDropped formType brick =
    case formType of
        D ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs unevenRowColumnCells brick then
                True

            else
                False

        E ->
            if isUnEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isEvenIsRightAndBaseColumnIs (evenRowColumnCells - 1) brick then
                True

            else
                False
