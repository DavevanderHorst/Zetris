module Functions.Square exposing (..)

import Functions.Base exposing (isEven, makePlayFieldDictKey)
import Functions.BrickForm exposing (BrickForm(..), FormType(..), switchFormType)
import Functions.BrickMoveDirection exposing (BrickMoveDirection(..))
import Models exposing (BrickModel)
import PlayFieldSizes exposing (evenRowColumnCells, unevenRowColumnCells)



--                 .
-- Square A Form  ..
--                 .
--
-- Square B Form ..
--                ..
--
-- Square C Form   ..
--                ..


doesSquareBumpWallWhenDropped : FormType -> BrickModel -> Bool
doesSquareBumpWallWhenDropped formType brick =
    case formType of
        A ->
            if isEven brick.baseRow then
                if brick.direction == Left && brick.baseColumn == 1 then
                    True

                else if brick.direction == Right && brick.baseColumn == evenRowColumnCells then
                    True

                else
                    False

            else
                False

        B ->
            if isEven brick.baseRow then
                if brick.direction == Left && brick.baseColumn == 1 then
                    True

                else
                    False

            else if brick.direction == Right && brick.baseColumn == unevenRowColumnCells then
                True

            else
                False

        C ->
            if isEven brick.baseRow then
                if brick.direction == Right && brick.baseColumn == evenRowColumnCells - 1 then
                    True

                else
                    False

            else if brick.direction == Left && brick.baseColumn == 1 then
                True

            else
                False


switchSquareBrickForm : FormType -> BrickModel -> BrickModel
switchSquareBrickForm formType brick =
    let
        newFormType =
            switchFormType formType

        newBrickDictKeys =
            createSquarePlayFieldDictKeys brick.baseRow brick.baseColumn newFormType
    in
    { brick | form = Square newFormType, playFieldDictKeys = newBrickDictKeys }



-- Create keys for SQUARE in play field


createSquarePlayFieldDictKeys : Int -> Int -> FormType -> List String
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


createASquarePlayFieldDictKeys : Int -> Int -> String -> List String
createASquarePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        upRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        upRowNumber =
            startRowNumber - 1

        secondKey =
            makePlayFieldDictKey upRowNumber upRowLeftColumnNumber

        thirdKey =
            makePlayFieldDictKey upRowNumber (upRowLeftColumnNumber + 1)

        fourthKey =
            makePlayFieldDictKey (startRowNumber - 2) startColumnNumber
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createBSquarePlayFieldDictKeys : Int -> Int -> String -> List String
createBSquarePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber - 1)

        upRowRightColumnNumber =
            if isEven startRowNumber then
                startColumnNumber

            else
                startColumnNumber - 1

        upRowNumber =
            startRowNumber - 1

        thirdKey =
            makePlayFieldDictKey upRowNumber upRowRightColumnNumber

        fourthKey =
            makePlayFieldDictKey upRowNumber (upRowRightColumnNumber - 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]


createCSquarePlayFieldDictKeys : Int -> Int -> String -> List String
createCSquarePlayFieldDictKeys startRowNumber startColumnNumber firstKey =
    let
        secondKey =
            makePlayFieldDictKey startRowNumber (startColumnNumber + 1)

        upRowLeftColumnNumber =
            if isEven startRowNumber then
                startColumnNumber + 1

            else
                startColumnNumber

        upRowNumber =
            startRowNumber - 1

        thirdKey =
            makePlayFieldDictKey upRowNumber upRowLeftColumnNumber

        fourthKey =
            makePlayFieldDictKey upRowNumber (upRowLeftColumnNumber + 1)
    in
    [ firstKey, secondKey, thirdKey, fourthKey ]
