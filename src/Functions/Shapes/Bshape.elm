module Functions.Shapes.Bshape exposing (..)

import Constants.PlayFieldSizes exposing (evenRowColumnCells, unevenRowColumnCells)
import Functions.Base exposing (isEven)
import Functions.BrickForm exposing (BrickForm(..), SixFormType(..), switchSixFormType)
import Functions.PlayFieldDictKeys exposing (makePlayFieldDictKey)
import Functions.Shapes.Comparisons exposing (isEvenIsLeftAndBaseColumnIs, isEvenIsRightAndBaseColumnIs, isUnEvenIsLeftAndBaseColumnIs, isUnEvenIsRightAndBaseColumnIs)
import Models exposing (BrickModel)


switchBShapeBrickForm : SixFormType -> BrickModel -> BrickModel
switchBShapeBrickForm formType brick =
    let
        newFormType =
            switchSixFormType formType

        newBrickDictKeys =
            createBShapePlayFieldDictKeys brick.baseRow brick.baseColumn newFormType
    in
    { brick | form = BShape newFormType, playFieldDictKeys = newBrickDictKeys }


createBShapePlayFieldDictKeys : Int -> Int -> SixFormType -> List String
createBShapePlayFieldDictKeys startRowNumber startColumnNumber formType =
    let
        baseKey =
            makePlayFieldDictKey startRowNumber startColumnNumber
    in
    case formType of
        F ->
            createFBShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        G ->
            createGBShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        H ->
            createHBShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        I ->
            createIBShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        J ->
            createJBShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        K ->
            createKBShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey


createFBShapePlayFieldDictKeys : Int -> Int -> String -> List String
createFBShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        downRowColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 1) downRowColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createGBShapePlayFieldDictKeys : Int -> Int -> String -> List String
createGBShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        ( downRowColumnNumber, upRowColumnNumber ) =
            if isEven startRowNumber then
                ( startColumnNumber, startColumnNumber + 1 )

            else
                ( startColumnNumber - 1, startColumnNumber )

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        thirdKey =
            makePlayFieldDictKey (startRowNumber + 1) upRowColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 1) downRowColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createHBShapePlayFieldDictKeys : Int -> Int -> String -> List String
createHBShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        ( upRowColumnNumber, downRowColumnNumber ) =
            if isEven startRowNumber then
                ( startColumnNumber, startColumnNumber + 1 )

            else
                ( startColumnNumber - 1, startColumnNumber )

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) downRowColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber + 1) upRowColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) (upRowColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createIBShapePlayFieldDictKeys : Int -> Int -> String -> List String
createIBShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) upRowColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createJBShapePlayFieldDictKeys : Int -> Int -> String -> List String
createJBShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        ( downRowColNumber, upRowColumnNumber ) =
            if isEven startRowNumber then
                ( startColumnNumber, startColumnNumber + 1 )

            else
                ( startColumnNumber - 1, startColumnNumber )

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) downRowColNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber + 1) upRowColumnNumber

        fourthKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createKBShapePlayFieldDictKeys : Int -> Int -> String -> List String
createKBShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upDownRowColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) upDownRowColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber - 1) (upDownRowColumnNumber + 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) upDownRowColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


doesBShapeBumpWallWhenDropped : SixFormType -> BrickModel -> Bool
doesBShapeBumpWallWhenDropped formType brick =
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

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 1) brick then
                True

            else
                False

        H ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
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
            if isUnEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else
                False

        K ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else
                False
