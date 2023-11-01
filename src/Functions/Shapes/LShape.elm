module Functions.Shapes.LShape exposing (..)

import Functions.Base exposing (isEven)
import Functions.BrickForm exposing (BrickForm(..), SixFormType(..), switchSixFormType)
import Functions.PlayFieldDictKeys exposing (makePlayFieldDictKey)
import Functions.Shapes.Comparisons exposing (isEvenIsLeftAndBaseColumnIs, isEvenIsRightAndBaseColumnIs, isUnEvenIsLeftAndBaseColumnIs, isUnEvenIsRightAndBaseColumnIs)
import Models exposing (BrickModel)
import PlayFieldSizes exposing (evenRowColumnCells, unevenRowColumnCells)


createLShapePlayFieldDictKeys : Int -> Int -> SixFormType -> List String
createLShapePlayFieldDictKeys startRowNumber startColumnNumber formType =
    let
        baseKey =
            makePlayFieldDictKey startRowNumber startColumnNumber
    in
    case formType of
        Aa ->
            createALShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        Bb ->
            createBLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        Cc ->
            createCLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        Dd ->
            createDLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        Ee ->
            createELShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey

        Ff ->
            createFLShapePlayFieldDictKeys startRowNumber startColumnNumber baseKey


createALShapePlayFieldDictKeys : Int -> Int -> String -> List String
createALShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upAndDownRowColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        secondKey =
            makePlayFieldDictKey (startRowNumber - 1) upAndDownRowColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber + 1) upAndDownRowColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 2) (startColumnNumber - 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createBLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createBLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
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


createCLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createCLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowColumnNumber =
            if isEven startRowNumber then
                startColumnNumber - 1

            else
                startColumnNumber - 2

        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        thirdKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 1) upRowColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createDLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createDLShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        ( downRowColumnNumber, upRowColumnNumber ) =
            if isEven startRowNumber then
                ( startColumnNumber + 1, startColumnNumber )

            else
                ( startColumnNumber, startColumnNumber - 1 )

        secondKey =
            makePlayFieldDictKey (startRowNumber + 1) downRowColumnNumber

        thirdKey =
            makePlayFieldDictKey (startRowNumber - 1) upRowColumnNumber

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 2) startColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createELShapePlayFieldDictKeys : Int -> Int -> String -> List String
createELShapePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        ( downRowColumnNumber, upRowColumnNumber ) =
            if isEven startRowNumber then
                ( startColumnNumber, startColumnNumber + 1 )

            else
                ( startColumnNumber - 1, startColumnNumber )

        secondKey =
            makePlayFieldDictKey (startRowNumber + 1) downRowColumnNumber

        oneUpRowNumber =
            startRowNumber - 1

        thirdKey =
            makePlayFieldDictKey oneUpRowNumber upRowColumnNumber

        fourthKey =
            makePlayFieldDictKey oneUpRowNumber (upRowColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createFLShapePlayFieldDictKeys : Int -> Int -> String -> List String
createFLShapePlayFieldDictKeys startRowNumber startColumnNumber _ =
    let
        upRowColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        firstKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        upRowNumber =
            startRowNumber - 1

        secondKey =
            makePlayFieldDictKey upRowNumber upRowColumnNumber

        thirdKey =
            makePlayFieldDictKey upRowNumber (upRowColumnNumber - 1)

        fourthKey =
            makePlayFieldDictKey upRowNumber (upRowColumnNumber - 2)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


doesLShapeBumpWallWhenDropped : SixFormType -> BrickModel -> Bool
doesLShapeBumpWallWhenDropped formType brick =
    case formType of
        Aa ->
            if isUnEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs unevenRowColumnCells brick then
                True

            else
                False

        Bb ->
            if isUnEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 1) brick then
                True

            else
                False

        Cc ->
            if isEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 1) brick then
                True

            else
                False

        Dd ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isEvenIsRightAndBaseColumnIs evenRowColumnCells brick then
                True

            else
                False

        Ee ->
            if isEvenIsLeftAndBaseColumnIs 1 brick then
                True

            else if isEvenIsRightAndBaseColumnIs (evenRowColumnCells - 1) brick then
                True

            else
                False

        Ff ->
            if isEvenIsLeftAndBaseColumnIs 2 brick then
                True

            else if isUnEvenIsRightAndBaseColumnIs (unevenRowColumnCells - 1) brick then
                True

            else
                False


switchLShapeBrickForm : SixFormType -> BrickModel -> BrickModel
switchLShapeBrickForm formType brick =
    let
        newFormType =
            switchSixFormType formType

        newBrickDictKeys =
            createLShapePlayFieldDictKeys brick.baseRow brick.baseColumn newFormType
    in
    { brick | form = LShape newFormType, playFieldDictKeys = newBrickDictKeys }
