module Functions.Shapes.PShape exposing (..)

import Constants.PlayFieldSizes exposing (evenRowColumnCells, unevenRowColumnCells)
import Functions.Base exposing (isEven)
import Functions.BrickForm exposing (BrickForm(..), SixFormType(..), switchSixFormTypeRight)
import Functions.PlayFieldDictKeys exposing (makePlayFieldDictKey)
import Functions.Shapes.Comparisons exposing (isEvenIsLeftAndBaseColumnIs, isEvenIsRightAndBaseColumnIs, isUnEvenIsLeftAndBaseColumnIs, isUnEvenIsRightAndBaseColumnIs)
import Models exposing (BrickModel)


switchPShapeBrickForm : SixFormType -> BrickModel -> BrickModel
switchPShapeBrickForm formType brick =
    let
        newFormType =
            switchSixFormTypeRight formType

        newBrickDictKeys =
            createPShapePlayFieldDictKeys brick.baseRow brick.baseColumn newFormType
    in
    { brick | form = PShape newFormType, playFieldDictKeys = newBrickDictKeys }


createPShapePlayFieldDictKeys : Int -> Int -> SixFormType -> List String
createPShapePlayFieldDictKeys startRowNumber startColumnNumber formType =
    let
        baseKey =
            makePlayFieldDictKey startRowNumber startColumnNumber
    in
    case formType of
        A ->
            createFPShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        B ->
            createGPShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        C ->
            createHPShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        D ->
            createIPShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        E ->
            createJPShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        F ->
            createKPShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey


createFPShapePlayFieldDictKeys : Int -> Int -> String -> List String
createFPShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        downRowColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 1) downRowColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createGPShapePlayFieldDictKeys : Int -> Int -> String -> List String
createGPShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        ( upRowColumnNumber, downRowColumnNumber ) =
            if isEven startRowNumber then
                ( startColumnNumber + 1, startColumnNumber )

            else
                ( startColumnNumber, startColumnNumber - 1 )

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) downRowColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber - 1) (downRowColumnNumber + 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) upRowColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createHPShapePlayFieldDictKeys : Int -> Int -> String -> List String
createHPShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
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
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createIPShapePlayFieldDictKeys : Int -> Int -> String -> List String
createIPShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) upRowColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createJPShapePlayFieldDictKeys : Int -> Int -> String -> List String
createJPShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upDownRowColNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) upDownRowColNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber + 1) upDownRowColNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) (upDownRowColNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createKPShapePlayFieldDictKeys : Int -> Int -> String -> List String
createKPShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        ( upRowColumnNumber, downRowColumnNumber ) =
            if isEven startRowNumber then
                ( startColumnNumber, startColumnNumber + 1 )

            else
                ( startColumnNumber - 1, startColumnNumber )

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) downRowColumnNumber

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) upRowColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


doesPShapeBumpWallWhenDropped : SixFormType -> BrickModel -> Bool
doesPShapeBumpWallWhenDropped formType brick =
    case formType of
        A ->
            if isUnEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 1) brick then
                True

            else
                False

        B ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else
                False

        C ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 1) brick then
                True

            else
                False

        D ->
            if isUnEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 1) brick then
                True

            else
                False

        E ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else
                False

        F ->
            if isUnEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else
                False
