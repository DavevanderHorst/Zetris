module Functions.Brick exposing (..)

import Functions.Base exposing (isEven)
import Functions.BrickForm exposing (BrickForm(..), ThreeFormType)
import Functions.BrickMoveDirection exposing (BrickMoveDirection(..), switchBrickMoveDirection)
import Functions.PlayFieldDictKeys exposing (getRowNumberFromPlayFieldDictKey)
import Functions.Shapes.LShape exposing (createLShapePlayFieldDictKeys, doesLShapeBumpWallWhenDropped, switchLShapeBrickForm)
import Functions.Shapes.Square exposing (createSquarePlayFieldDictKeys, doesSquareBumpWallWhenDropped, switchSquareBrickForm)
import Models exposing (BrickModel, Color(..))


switchBrickForm : BrickModel -> BrickModel
switchBrickForm brick =
    case brick.form of
        Square formType ->
            switchSquareBrickForm formType brick

        LShape formType ->
            switchLShapeBrickForm formType brick


getCurrentRowsFromBrick : BrickModel -> List Int
getCurrentRowsFromBrick brick =
    List.foldl addUniqueRowNumber [] brick.playFieldDictKeys


addUniqueRowNumber : String -> List Int -> List Int
addUniqueRowNumber key rowList =
    let
        rowNumber =
            getRowNumberFromPlayFieldDictKey key
    in
    if List.member rowNumber rowList then
        rowList

    else
        rowNumber :: rowList


dropBrickModel : BrickModel -> BrickModel
dropBrickModel brick =
    -- we drop the brick one row, and change direction when bumping against wall
    -- we check if it is possible to drop later
    let
        isEvenRowNumber =
            isEven brick.baseRow

        updatedBrick =
            changeBrickDirectionWhenBumpingWallsWhenDropped brick

        droppedRowNumber =
            brick.baseRow + 1

        droppedColumnNumber =
            case updatedBrick.direction of
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
            createPlayFieldDictKeysForBrickForm droppedRowNumber droppedColumnNumber brick.form
    in
    { updatedBrick | baseRow = droppedRowNumber, baseColumn = droppedColumnNumber, playFieldDictKeys = newDictKeys }


changeBrickDirectionWhenBumpingWallsWhenDropped : BrickModel -> BrickModel
changeBrickDirectionWhenBumpingWallsWhenDropped brick =
    let
        directionForDropping =
            case brick.form of
                Square formType ->
                    if doesSquareBumpWallWhenDropped formType brick then
                        switchBrickMoveDirection brick.direction

                    else
                        brick.direction

                LShape formType ->
                    if doesLShapeBumpWallWhenDropped formType brick then
                        switchBrickMoveDirection brick.direction

                    else
                        brick.direction
    in
    { brick | direction = directionForDropping }


isThereCurrentActiveBrick : Maybe BrickModel -> Result String BrickModel
isThereCurrentActiveBrick maybeBrick =
    case maybeBrick of
        Nothing ->
            Err "?"

        Just brick ->
            if brick.isActive then
                Ok brick

            else
                Err "?"


moveBrickModelLeft : BrickModel -> BrickModel
moveBrickModelLeft brick =
    moveBrickModel Left brick


moveBrickModelRight : BrickModel -> BrickModel
moveBrickModelRight brick =
    moveBrickModel Right brick


moveBrickModel : BrickMoveDirection -> BrickModel -> BrickModel
moveBrickModel direction brick =
    let
        newColumnNumber =
            case direction of
                Left ->
                    brick.baseColumn - 1

                Right ->
                    brick.baseColumn + 1

        newDictKeys =
            createPlayFieldDictKeysForBrickForm brick.baseRow newColumnNumber brick.form
    in
    { brick | baseColumn = newColumnNumber, playFieldDictKeys = newDictKeys }


isBrickActive : Maybe BrickModel -> Bool
isBrickActive maybeBrick =
    case maybeBrick of
        Nothing ->
            False

        Just brick ->
            brick.isActive


getStartRowNumberForBrickForm : BrickForm -> Int
getStartRowNumberForBrickForm form =
    case form of
        Square _ ->
            3

        LShape _ ->
            3


createPlayFieldDictKeysForBrickForm : Int -> Int -> BrickForm -> List String
createPlayFieldDictKeysForBrickForm row col form =
    case form of
        Square formType ->
            createSquarePlayFieldDictKeys row col formType

        LShape formType ->
            createLShapePlayFieldDictKeys row col formType
