module Functions.Shapes.LShape exposing (..)

import Constants.PlayFieldSizes exposing (evenRowColumnCells, unevenRowColumnCells)
import Functions.Base exposing (isEven)
import Functions.BrickForm exposing (BrickForm(..), SixFormType(..), switchSixFormTypeRight)
import Functions.PlayFieldDictKeys exposing (makePlayFieldDictKey)
import Functions.Shapes.Comparisons exposing (isEvenIsLeftAndBaseColumnIs, isEvenIsRightAndBaseColumnIs, isUnEvenIsLeftAndBaseColumnIs, isUnEvenIsRightAndBaseColumnIs)
import Models exposing (BrickModel)


switchLShapeBrickForm : SixFormType -> BrickModel -> BrickModel
switchLShapeBrickForm newFormType brick =
    let
        newBrickDictKeys =
            createLShapePlayFieldDictKeys brick.baseRow brick.baseColumn newFormType
    in
    { brick | form = LShape newFormType, playFieldDictKeys = newBrickDictKeys }


createLShapePlayFieldDictKeys : Int -> Int -> SixFormType -> List String
createLShapePlayFieldDictKeys startRowNumber startColumnNumber formType =
    let
        baseKey =
            makePlayFieldDictKey startRowNumber startColumnNumber
    in
    case formType of
        A ->
            createALShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        B ->
            createBLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        C ->
            createCLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        D ->
            createDLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        E ->
            createELShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        F ->
            createFLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey


createALShapePlayFieldDictKeys : Int -> Int -> String -> List String
createALShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        downRowRightColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 2

            else
                startColumnNumber + 1

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 1) downRowRightColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createBLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createBLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        downRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) downRowLeftColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber + 1) (downRowLeftColumnNumber + 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) (downRowLeftColumnNumber + 2)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createCLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createCLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        downRowRightColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) downRowRightColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber + 1) (downRowRightColumnNumber - 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 2) startColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createDLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createDLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) (upRowLeftColumnNumber - 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createELShapePlayFieldDictKeys : Int -> Int -> String -> List String
createELShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) upRowLeftColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber - 1) (upRowLeftColumnNumber - 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) (upRowLeftColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createFLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createFLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        downRowRightColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) downRowRightColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber - 2) startColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) (downRowRightColumnNumber - 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


doesLShapeBumpWallWhenDropped : SixFormType -> BrickModel -> Bool
doesLShapeBumpWallWhenDropped formType brick =
    case formType of
        A ->
            if isUnEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isEvenIsRightAndBaseColumnIs (evenRowColumnCells - 1) brick then
                True

            else
                False

        B ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isEvenIsRightAndBaseColumnIs (evenRowColumnCells - 1) brick then
                True

            else
                False

        C ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else
                False

        D ->
            if isEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 1) brick then
                True

            else
                False

        E ->
            if isEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else
                False

        F ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else
                False
