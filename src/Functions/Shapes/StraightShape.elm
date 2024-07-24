module Functions.Shapes.StraightShape exposing (..)

import Constants.PlayFieldSizes exposing (evenRowColumnCells, unevenRowColumnCells)
import Functions.Base exposing (isEven)
import Functions.BrickForm exposing (BrickForm(..), SixFormType(..), switchSixFormTypeRight)
import Functions.PlayFieldDictKeys exposing (makePlayFieldDictKey)
import Functions.Shapes.Comparisons exposing (isEvenIsLeftAndBaseColumnIs, isEvenIsRightAndBaseColumnIs, isUnEvenIsLeftAndBaseColumnIs, isUnEvenIsRightAndBaseColumnIs)
import Models exposing (BrickModel)


switchStraightShape : SixFormType -> BrickModel -> BrickModel
switchStraightShape newFormType brick =
    let
        newBrickDictKeys =
            createStraightPlayFieldDictKeys brick.baseRow brick.baseColumn newFormType
    in
    { brick | form = Straight newFormType, playFieldDictKeys = newBrickDictKeys }


createStraightPlayFieldDictKeys : Int -> Int -> SixFormType -> List String
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

        D ->
            createDStraightPlayFieldDictKeys startRowNumber startColumnNumber baseKey

        E ->
            createEStraightPlayFieldDictKeys startRowNumber startColumnNumber baseKey

        F ->
            createFStraightPlayFieldDictKeys startRowNumber startColumnNumber baseKey


createAStraightPlayFieldDictKeys : Int -> Int -> String -> List String
createAStraightPlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        fourthKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 2)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createBStraightPlayFieldDictKeys : Int -> Int -> String -> List String
createBStraightPlayFieldDictKeys startRowNumber startColumnNumber firstKey =
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
            makePlayFieldDictKey (startRowNumber + 2) (startColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createCStraightPlayFieldDictKeys : Int -> Int -> String -> List String
createCStraightPlayFieldDictKeys startRowNumber startColumnNumber firstKey =
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
            makePlayFieldDictKey (startRowNumber + 2) (startColumnNumber - 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createDStraightPlayFieldDictKeys : Int -> Int -> String -> List String
createDStraightPlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 2)

        fourthKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createEStraightPlayFieldDictKeys : Int -> Int -> String -> List String
createEStraightPlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        downRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) downRowLeftColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber - 2) (startColumnNumber - 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) (downRowLeftColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createFStraightPlayFieldDictKeys : Int -> Int -> String -> List String
createFStraightPlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        downRowRightColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) downRowRightColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber - 2) (startColumnNumber + 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber + 1) (downRowRightColumnNumber - 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


doesStraightBumpWallWhenDropped : SixFormType -> BrickModel -> Bool
doesStraightBumpWallWhenDropped formType brick =
    case formType of
        A ->
            if isUnEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 2) brick then
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
            if isUnEvenIsLeftAndBaseColumnIs 3 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 1) brick then
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
