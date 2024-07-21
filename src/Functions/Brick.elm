module Functions.Brick exposing (..)

import Functions.Base exposing (isEven)
import Functions.BrickForm exposing (BrickForm(..))
import Functions.BrickMoveDirection exposing (BrickMoveDirection(..), switchBrickMoveDirection)
import Functions.PlayFieldDictKeys exposing (getRowNumberFromPlayFieldDictKey)
import Functions.Shapes.Bshape exposing (createBShapePlayFieldDictKeys, doesBShapeBumpWallWhenDropped, switchBShapeBrickForm)
import Functions.Shapes.LShape exposing (createLShapePlayFieldDictKeys, doesLShapeBumpWallWhenDropped, switchLShapeBrickForm)
import Functions.Shapes.PShape exposing (createPShapePlayFieldDictKeys, doesPShapeBumpWallWhenDropped, switchPShapeBrickForm)
import Functions.Shapes.ReversedLShape exposing (createReversedLShapePlayFieldDictKeys, doesReversedLShapeBumpWallWhenDropped, switchReversedLShapeBrickForm)
import Functions.Shapes.SShape exposing (createSShapePlayFieldDictKeys, doesSShapeBumpWallWhenDropped, switchSShapeBrickForm)
import Functions.Shapes.Square exposing (createSquarePlayFieldDictKeys, doesSquareBumpWallWhenDropped, switchSquareBrickForm)
import Functions.Shapes.StraightShape exposing (createStraightPlayFieldDictKeys, doesStraightBumpWallWhenDropped, switchStraightShape)
import Functions.Shapes.ZShape exposing (createZShapePlayFieldDictKeys, doesZShapeBumpWallWhenDropped, switchZShapeBrickForm)
import Models exposing (BrickModel, Color(..))


switchBrickForm : BrickModel -> BrickModel
switchBrickForm brick =
    case brick.form of
        Square formType ->
            switchSquareBrickForm formType brick

        LShape formType ->
            switchLShapeBrickForm formType brick

        ReversedLShape formType ->
            switchReversedLShapeBrickForm formType brick

        Straight formType ->
            switchStraightShape formType brick

        SShape formType ->
            switchSShapeBrickForm formType brick

        ZShape formType ->
            switchZShapeBrickForm formType brick

        BShape formType ->
            switchBShapeBrickForm formType brick

        PShape formType ->
            switchPShapeBrickForm formType brick


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
        switchDirectionForDropping =
            case brick.form of
                Square formType ->
                    doesSquareBumpWallWhenDropped formType brick

                LShape formType ->
                    doesLShapeBumpWallWhenDropped formType brick

                ReversedLShape formType ->
                    doesReversedLShapeBumpWallWhenDropped formType brick

                Straight formType ->
                    doesStraightBumpWallWhenDropped formType brick

                SShape formType ->
                    doesSShapeBumpWallWhenDropped formType brick

                ZShape formType ->
                    doesZShapeBumpWallWhenDropped formType brick

                BShape formType ->
                    doesBShapeBumpWallWhenDropped formType brick

                PShape formType ->
                    doesPShapeBumpWallWhenDropped formType brick

        directionForDropping =
            if switchDirectionForDropping then
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


createPlayFieldDictKeysForBrickForm : Int -> Int -> BrickForm -> List String
createPlayFieldDictKeysForBrickForm row col form =
    case form of
        Square formType ->
            createSquarePlayFieldDictKeys row col formType

        LShape formType ->
            createLShapePlayFieldDictKeys row col formType

        ReversedLShape formType ->
            createReversedLShapePlayFieldDictKeys row col formType

        Straight formType ->
            createStraightPlayFieldDictKeys row col formType

        SShape formType ->
            createSShapePlayFieldDictKeys row col formType

        ZShape formType ->
            createZShapePlayFieldDictKeys row col formType

        BShape formType ->
            createBShapePlayFieldDictKeys row col formType

        PShape formType ->
            createPShapePlayFieldDictKeys row col formType
