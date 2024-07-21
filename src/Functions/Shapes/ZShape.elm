module Functions.Shapes.ZShape exposing (..)

import Constants.PlayFieldSizes exposing (evenRowColumnCells, unevenRowColumnCells)
import Functions.Base exposing (isEven)
import Functions.BrickForm exposing (BrickForm(..), SixFormType(..), switchSixFormTypeRight)
import Functions.PlayFieldDictKeys exposing (makePlayFieldDictKey)
import Functions.Shapes.Comparisons exposing (isEvenIsLeftAndBaseColumnIs, isEvenIsRightAndBaseColumnIs, isUnEvenIsLeftAndBaseColumnIs, isUnEvenIsRightAndBaseColumnIs)
import Models exposing (BrickModel)


switchZShapeBrickForm : SixFormType -> BrickModel -> BrickModel
switchZShapeBrickForm formType brick =
    let
        newFormType =
            switchSixFormTypeRight formType

        newBrickDictKeys =
            createZShapePlayFieldDictKeys brick.baseRow brick.baseColumn newFormType
    in
    { brick | form = ZShape newFormType, playFieldDictKeys = newBrickDictKeys }


createZShapePlayFieldDictKeys : Int -> Int -> SixFormType -> List String
createZShapePlayFieldDictKeys startRowNumber startColumnNumber formType =
    let
        baseKey =
            makePlayFieldDictKey startRowNumber startColumnNumber
    in
    case formType of
        A ->
            createAZShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        B ->
            createBZShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        C ->
            createCZShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        D ->
            createDZShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        E ->
            createEZShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        F ->
            createFZShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey


createAZShapePlayFieldDictKeys : Int -> Int -> String -> List String
createAZShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upDownRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) upDownRowLeftColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber + 1) upDownRowLeftColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 2) startColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createBZShapePlayFieldDictKeys : Int -> Int -> String -> List String
createBZShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        downRowRightColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) downRowRightColumnNumber

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) (downRowRightColumnNumber - 2)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createCZShapePlayFieldDictKeys : Int -> Int -> String -> List String
createCZShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        downRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        thirdKey =
            makePlayFieldDictKey (startRowNumber - 1) downRowLeftColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 1) (downRowLeftColumnNumber - 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createDZShapePlayFieldDictKeys : Int -> Int -> String -> List String
createDZShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upDownRowRightColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        secondKey =
            makePlayFieldDictKey (startRowNumber + 1) upDownRowRightColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber - 1) upDownRowRightColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 2) startColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createEZShapePlayFieldDictKeys : Int -> Int -> String -> List String
createEZShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey (startRowNumber + 1) upRowLeftColumnNumber

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 1) (upRowLeftColumnNumber + 2)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createFZShapePlayFieldDictKeys : Int -> Int -> String -> List String
createFZShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowRightColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        thirdKey =
            makePlayFieldDictKey (startRowNumber + 1) upRowRightColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) (upRowRightColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


doesZShapeBumpWallWhenDropped : SixFormType -> BrickModel -> Bool
doesZShapeBumpWallWhenDropped formType brick =
    case formType of
        A ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs unevenRowColumnCells brick then
                True

            else
                False

        B ->
            if isEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else
                False

        C ->
            if isEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 1) brick then
                True

            else
                False

        D ->
            if isUnEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else
                False

        E ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isEvenIsRightAndBaseColumnIs (evenRowColumnCells - 1) brick then
                True

            else
                False

        F ->
            if isUnEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isEvenIsRightAndBaseColumnIs (evenRowColumnCells - 1) brick then
                True

            else
                False
