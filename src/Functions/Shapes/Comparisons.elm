module Functions.Shapes.Comparisons exposing (..)

import Functions.Base exposing (isEven)
import Functions.BrickMoveDirection exposing (BrickMoveDirection(..))
import Models exposing (BrickModel)


isEvenIsLeftAndBaseColumnIs : Int -> BrickModel -> Bool
isEvenIsLeftAndBaseColumnIs col brick =
    if isEven brick.baseRow then
        if isDirectionAndIsColNumber Left col brick then
            True

        else
            False

    else
        False


isUnEvenIsLeftAndBaseColumnIs : Int -> BrickModel -> Bool
isUnEvenIsLeftAndBaseColumnIs col brick =
    if isEven brick.baseRow then
        False

    else if isDirectionAndIsColNumber Left col brick then
        True

    else
        False


isEvenIsRightAndBaseColumnIs : Int -> BrickModel -> Bool
isEvenIsRightAndBaseColumnIs col brick =
    if isEven brick.baseRow then
        if isDirectionAndIsColNumber Right col brick then
            True

        else
            False

    else
        False


isUnEvenIsRightAndBaseColumnIs : Int -> BrickModel -> Bool
isUnEvenIsRightAndBaseColumnIs col brick =
    if isEven brick.baseRow then
        False

    else if isDirectionAndIsColNumber Right col brick then
        True

    else
        False


isDirectionAndIsColNumber : BrickMoveDirection -> Int -> BrickModel -> Bool
isDirectionAndIsColNumber direction colNumber brick =
    if brick.direction == direction && brick.baseColumn == colNumber then
        True

    else
        False
