module Functions.Shapes.Square exposing (..)

import Constants.PlayFieldSizes exposing (evenRowColumnCells, unevenRowColumnCells)
import Functions.Base exposing (isEven)
import Functions.BrickForm exposing (BrickForm(..), SixFormType(..), switchSixFormTypeLeft, switchSixFormTypeRight)
import Functions.PlayFieldDictKeys exposing (makePlayFieldDictKey)
import Functions.Shapes.Comparisons exposing (isEvenIsLeftAndBaseColumnIs, isEvenIsRightAndBaseColumnIs, isUnEvenIsLeftAndBaseColumnIs, isUnEvenIsRightAndBaseColumnIs)
import Models exposing (BrickModel, SwitchDirection(..))


switchSquareBrickForm : SixFormType -> BrickModel -> BrickModel
switchSquareBrickForm newFormType brick =
    let
        newBrickDictKeys =
            createSquarePlayFieldDictKeys brick.baseRow brick.baseColumn newFormType
    in
    { brick | form = Square newFormType, playFieldDictKeys = newBrickDictKeys }


createSquarePlayFieldDictKeys : Int -> Int -> SixFormType -> List String
createSquarePlayFieldDictKeys startRowNumber startColumnNumber formType =
    let
        baseKey =
            makePlayFieldDictKey startRowNumber startColumnNumber
    in
    case formType of
        A ->
            createASquarePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        B ->
            createBSquarePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        C ->
            createCSquarePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        D ->
            createDSquarePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        E ->
            createESquarePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        F ->
            createFSquarePlayFieldDictKeys startRowNumber startColumnNumber baseKey


createASquarePlayFieldDictKeys : Int -> Int -> String -> List String
createASquarePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upDownRowRightColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) upDownRowRightColumnNumber

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) upDownRowRightColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createBSquarePlayFieldDictKeys : Int -> Int -> String -> List String
createBSquarePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        upRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        upRowNumber =
            startRowNumber + 1

        thirdKey =
            makePlayFieldDictKey upRowNumber upRowLeftColumnNumber

        fourthKey =
            makePlayFieldDictKey upRowNumber (upRowLeftColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createCSquarePlayFieldDictKeys : Int -> Int -> String -> List String
createCSquarePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        upRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        upRowNumber =
            startRowNumber + 1

        thirdKey =
            makePlayFieldDictKey upRowNumber upRowLeftColumnNumber

        fourthKey =
            makePlayFieldDictKey upRowNumber (upRowLeftColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createDSquarePlayFieldDictKeys : Int -> Int -> String -> List String
createDSquarePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        upDownRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        thirdKey =
            makePlayFieldDictKey (startRowNumber + 1) upDownRowLeftColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 1) upDownRowLeftColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createESquarePlayFieldDictKeys : Int -> Int -> String -> List String
createESquarePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        downRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        downRow =
            startRowNumber - 1

        thirdKey =
            makePlayFieldDictKey downRow downRowLeftColumnNumber

        fourthKey =
            makePlayFieldDictKey downRow (downRowLeftColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createFSquarePlayFieldDictKeys : Int -> Int -> String -> List String
createFSquarePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        downRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        downRow =
            startRowNumber - 1

        thirdKey =
            makePlayFieldDictKey downRow downRowLeftColumnNumber

        fourthKey =
            makePlayFieldDictKey downRow (downRowLeftColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


doesSquareBumpWallWhenDropped : SixFormType -> BrickModel -> Bool
doesSquareBumpWallWhenDropped formType brick =
    case formType of
        A ->
            if isUnEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 1) brick then
                True

            else
                False

        B ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 1) brick then
                True

            else
                False

        C ->
            if isUnEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else
                False

        D ->
            if isUnEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs unevenRowColumnCells brick then
                True

            else
                False

        E ->
            if isUnEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else
                False

        F ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 1) brick then
                True

            else
                False
