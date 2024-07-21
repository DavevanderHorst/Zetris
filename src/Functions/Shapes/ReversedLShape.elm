module Functions.Shapes.ReversedLShape exposing (..)

import Constants.PlayFieldSizes exposing (evenRowColumnCells, unevenRowColumnCells)
import Functions.Base exposing (isEven)
import Functions.BrickForm exposing (BrickForm(..), SixFormType(..), switchSixFormTypeRight)
import Functions.PlayFieldDictKeys exposing (makePlayFieldDictKey)
import Functions.Shapes.Comparisons exposing (isEvenIsLeftAndBaseColumnIs, isEvenIsRightAndBaseColumnIs, isUnEvenIsLeftAndBaseColumnIs, isUnEvenIsRightAndBaseColumnIs)
import Models exposing (BrickModel)


switchReversedLShapeBrickForm : SixFormType -> BrickModel -> BrickModel
switchReversedLShapeBrickForm formType brick =
    let
        newFormType =
            switchSixFormTypeRight formType

        newBrickDictKeys =
            createReversedLShapePlayFieldDictKeys brick.baseRow brick.baseColumn newFormType
    in
    { brick | form = ReversedLShape newFormType, playFieldDictKeys = newBrickDictKeys }


createReversedLShapePlayFieldDictKeys : Int -> Int -> SixFormType -> List String
createReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber formType =
    let
        baseKey =
            makePlayFieldDictKey startRowNumber startColumnNumber
    in
    case formType of
        A ->
            createAReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        B ->
            createBReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        C ->
            createCReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        D ->
            createDReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        E ->
            createEReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        F ->
            createFReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey


createAReversedLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createAReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        downRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 1) (downRowLeftColumnNumber - 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createBReversedLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createBReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
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
            makePlayFieldDictKey (startRowNumber - 2) startColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createCReversedLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createCReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) (upRowLeftColumnNumber + 1)

        thirdKey =
            makePlayFieldDictKey (startRowNumber - 1) (upRowLeftColumnNumber + 2)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) upRowLeftColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createDReversedLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createDReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowRightColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) (upRowRightColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createEReversedLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createEReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
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
            makePlayFieldDictKey (startRowNumber + 2) startColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createFReversedLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createFReversedLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
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
            makePlayFieldDictKey (startRowNumber + 1) (downRowRightColumnNumber - 2)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


doesReversedLShapeBumpWallWhenDropped : SixFormType -> BrickModel -> Bool
doesReversedLShapeBumpWallWhenDropped formType brick =
    case formType of
        A ->
            if isEvenIsLeftAndBaseColumnIs 2 brick then
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

            else if isEvenIsRightAndBaseColumnIs (evenRowColumnCells - 1) brick then
                True

            else
                False

        D ->
            if isUnEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isEvenIsRightAndBaseColumnIs (evenRowColumnCells - 1) brick then
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
            if isEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else
                False
