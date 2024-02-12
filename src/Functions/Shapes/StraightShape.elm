module Functions.Shapes.StraightShape exposing (..)

import Constants.PlayFieldSizes exposing (evenRowColumnCells, unevenRowColumnCells)
import Functions.Base exposing (isEven)
import Functions.BrickForm exposing (BrickForm(..), ThreeFormType(..), switchThreeFormType)
import Functions.PlayFieldDictKeys exposing (makePlayFieldDictKey)
import Functions.Shapes.Comparisons exposing (isEvenIsLeftAndBaseColumnIs, isEvenIsRightAndBaseColumnIs, isUnEvenIsLeftAndBaseColumnIs, isUnEvenIsRightAndBaseColumnIs)
import Models exposing (BrickModel)


switchStraightShape : ThreeFormType -> BrickModel -> BrickModel
switchStraightShape formType brick =
    let
        newFormType =
            switchThreeFormType formType

        newBrickDictKeys =
            createStraightPlayFieldDictKeys brick.baseRow brick.baseColumn newFormType
    in
    { brick | form = Straight newFormType, playFieldDictKeys = newBrickDictKeys }


createStraightPlayFieldDictKeys : Int -> Int -> ThreeFormType -> List String
createStraightPlayFieldDictKeys startRowNumber startColumnNumber formType =
    let
        baseKey =
            makePlayFieldDictKey startRowNumber startColumnNumber
    in
    case formType of
        A ->
            createAStraightPlayFieldDictKeys startRowNumber startColumnNumber baseKey

        B ->
            createBStraightPlayFieldDictKeys startRowNumber startColumnNumber baseKey

        C ->
            createCStraightPlayFieldDictKeys startRowNumber startColumnNumber baseKey


createAStraightPlayFieldDictKeys : Int -> Int -> String -> List String
createAStraightPlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 2)

        fourthKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createBStraightPlayFieldDictKeys : Int -> Int -> String -> List String
createBStraightPlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) upRowLeftColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber + 1) (upRowLeftColumnNumber + 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 2) (startColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createCStraightPlayFieldDictKeys : Int -> Int -> String -> List String
createCStraightPlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowRightColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) upRowRightColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber + 1) (upRowRightColumnNumber - 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 2) (startColumnNumber - 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


doesStraightBumpWallWhenDropped : ThreeFormType -> BrickModel -> Bool
doesStraightBumpWallWhenDropped formType brick =
    case formType of
        A ->
            if isUnEvenIsLeftAndBaseColumnIs 3 brick then
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
            if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else if isUnEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else
                False
