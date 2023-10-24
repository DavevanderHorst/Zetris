module Functions.Brick exposing (..)

import Functions.Base exposing (isEven)
import Functions.Playfield exposing (createPlayFieldDictKeysForBrickForm)
import Models exposing (BrickForm(..), BrickModel, Color(..), Direction(..))
import PlayFieldSizes exposing (evenRowColumnCells, maximumRows)


dropBrickModel : BrickModel -> BrickModel
dropBrickModel brick =
    -- we drop the brick one row, and change direction when bumping against wall
    -- we check if it is possible to drop later
    let
        isEvenRowNumber =
            isEven brick.baseRow

        direction =
            changeBrickDirectionWhenBumpingWalls brick

        newRowNumber =
            brick.baseRow + 1

        newColumnNumber =
            case direction of
                Left ->
                    if isEvenRowNumber then
                        brick.baseColumn

                    else
                        brick.baseColumn - 1

                Right ->
                    if isEvenRowNumber then
                        brick.baseColumn + 1

                    else
                        brick.baseColumn

        newDictKeys =
            createPlayFieldDictKeysForBrickForm newRowNumber newColumnNumber brick.form
    in
    { brick | baseRow = newRowNumber, baseColumn = newColumnNumber, direction = direction, playFieldDictKeys = newDictKeys }


isBrickModelAtBottom : BrickModel -> Bool
isBrickModelAtBottom brick =
    if brick.baseRow == maximumRows then
        True

    else
        False


changeBrickDirectionWhenBumpingWalls : BrickModel -> Direction
changeBrickDirectionWhenBumpingWalls brick =
    case brick.form of
        Square ->
            if isEven brick.baseRow then
                if brick.direction == Left && brick.baseColumn == 1 then
                    Right

                else if brick.direction == Right && brick.baseColumn == evenRowColumnCells then
                    Left

                else
                    brick.direction

            else
                brick.direction


isBrickAtBottom : BrickModel -> Bool
isBrickAtBottom brick =
    case brick.form of
        Square ->
            if brick.baseRow == (maximumRows - 2) then
                True

            else
                False


getStartRowNumberForBrickForm : BrickForm -> Int
getStartRowNumberForBrickForm form =
    case form of
        Square ->
            3
