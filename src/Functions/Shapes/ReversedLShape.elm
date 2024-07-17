module Functions.Shapes.ReversedLShape exposing (..)

import Constants.PlayFieldSizes exposing (evenRowColumnCells, unevenRowColumnCells)
import Functions.Base exposing (isEven)
import Functions.BrickForm exposing (BrickForm(..), SixFormType(..), switchSixFormType)
import Functions.PlayFieldDictKeys exposing (makePlayFieldDictKey)
import Functions.Shapes.Comparisons exposing (isEvenIsLeftAndBaseColumnIs, isEvenIsRightAndBaseColumnIs, isUnEvenIsLeftAndBaseColumnIs, isUnEvenIsRightAndBaseColumnIs)
import Models exposing (BrickModel)


createReversedLShapePlayFieldDictKeys : Int -> Int -> SixFormType -> List String
createReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber formType =
    let
        baseKey =
            makePlayFieldDictKey startRowNumber startColumnNumber
    in
    case formType of
        F ->
            createFReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        G ->
            createGReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        H ->
            createHReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        I ->
            createIReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        J ->
            createJReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        K ->
            createKReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey


createFReversedLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createFReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        thirdKey =
            makePlayFieldDictKey (startRowNumber - 1) upRowColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 2) (startColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createGReversedLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createGReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 2)

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 1) upRowColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createHReversedLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createHReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upAndDownRowColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) upAndDownRowColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber + 1) upAndDownRowColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 2) (startColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createIReversedLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createIReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        downRowColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        thirdKey =
            makePlayFieldDictKey (startRowNumber + 1) downRowColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 2) (startColumnNumber - 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createJReversedLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createJReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        downRowColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 2)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) downRowColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createKReversedLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createKReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        downAndUpRowColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) downAndUpRowColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber + 1) downAndUpRowColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 2) (startColumnNumber - 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


doesReversedLShapeBumpWallWhenDropped : SixFormType -> BrickModel -> Bool
doesReversedLShapeBumpWallWhenDropped formType brick =
    case formType of
        F ->
            if isUnEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 1) brick then
                True

            else
                False

        G ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 2) brick then
                True

            else
                False

        H ->
            if isUnEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 1) brick then
                True

            else
                False

        I ->
            if isUnEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 1) brick then
                True

            else
                False

        J ->
            if isUnEvenIsLeftAndBaseColumnIs 3 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else
                False

        K ->
            if isUnEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs unevenRowColumnCells brick then
                True

            else
                False


switchReversedLShapeBrickForm : SixFormType -> BrickModel -> BrickModel
switchReversedLShapeBrickForm formType brick =
    let
        newFormType =
            switchSixFormType formType

        newBrickDictKeys =
            createReversedLShapePlayFieldDictKeys brick.baseRow brick.baseColumn newFormType
    in
    { brick | form = ReversedLShape newFormType, playFieldDictKeys = newBrickDictKeys }
