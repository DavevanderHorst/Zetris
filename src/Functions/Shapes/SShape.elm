module Functions.Shapes.SShape exposing (..)

import Constants.PlayFieldSizes exposing (evenRowColumnCells, unevenRowColumnCells)
import Functions.Base exposing (isEven)
import Functions.BrickForm exposing (BrickForm(..), SixFormType(..), switchSixFormTypeRight)
import Functions.PlayFieldDictKeys exposing (makePlayFieldDictKey)
import Functions.Shapes.Comparisons exposing (isEvenIsLeftAndBaseColumnIs, isEvenIsRightAndBaseColumnIs, isUnEvenIsLeftAndBaseColumnIs, isUnEvenIsRightAndBaseColumnIs)
import Models exposing (BrickModel)


switchSShapeBrickForm : SixFormType -> BrickModel -> BrickModel
switchSShapeBrickForm newFormType brick =
    let
        newBrickDictKeys =
            createSShapePlayFieldDictKeys brick.baseRow brick.baseColumn newFormType
    in
    { brick | form = SShape newFormType, playFieldDictKeys = newBrickDictKeys }


createSShapePlayFieldDictKeys : Int -> Int -> SixFormType -> List String
createSShapePlayFieldDictKeys startRowNumber startColumnNumber formType =
    let
        baseKey =
            makePlayFieldDictKey startRowNumber startColumnNumber
    in
    case formType of
        A ->
            createASShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        B ->
            createBSShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        C ->
            createCSShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        D ->
            createDSShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        E ->
            createESShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        F ->
            createFSShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey


createASShapePlayFieldDictKeys : Int -> Int -> String -> List String
createASShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upDownRowRightColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) upDownRowRightColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber + 1) upDownRowRightColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 2) startColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createBSShapePlayFieldDictKeys : Int -> Int -> String -> List String
createBSShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        downRowNumber =
            startRowNumber + 1

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        thirdKey =
            makePlayFieldDictKey downRowNumber upRowLeftColumnNumber

        fourthKey =
            makePlayFieldDictKey downRowNumber (upRowLeftColumnNumber - 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createCSShapePlayFieldDictKeys : Int -> Int -> String -> List String
createCSShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowRightColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        secondKey =
            makePlayFieldDictKey (startRowNumber + 1) upRowRightColumnNumber

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 1) (upRowRightColumnNumber - 2)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createDSShapePlayFieldDictKeys : Int -> Int -> String -> List String
createDSShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey (startRowNumber + 1) upRowLeftColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber - 1) upRowLeftColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 2) startColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createESShapePlayFieldDictKeys : Int -> Int -> String -> List String
createESShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        downRowRightColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        thirdKey =
            makePlayFieldDictKey (startRowNumber - 1) downRowRightColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 1) (downRowRightColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createFSShapePlayFieldDictKeys : Int -> Int -> String -> List String
createFSShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        downRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) downRowLeftColumnNumber

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) (downRowLeftColumnNumber + 2)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


doesSShapeBumpWallWhenDropped : SixFormType -> BrickModel -> Bool
doesSShapeBumpWallWhenDropped formType brick =
    case formType of
        A ->
            if isUnEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else
                False

        B ->
            if isEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 1) brick then
                True

            else
                False

        C ->
            if isEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else
                False

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

        F ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isEvenIsRightAndBaseColumnIs (evenRowColumnCells - 1) brick then
                True

            else
                False
