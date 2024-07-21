module Functions.Random exposing (..)

import Functions.BrickForm exposing (BrickForm, bShapeStartForm, lShapeStartForm, pShapeStartForm, reversedLShapeStartForm, sShapeStartForm, squareStartForm, straightStartForm, totalBrickTypes, zShapeStartForm)
import Functions.BrickMoveDirection exposing (BrickMoveDirection(..))
import Random


rollRandomBrickModel : Random.Generator ( Int, Int )
rollRandomBrickModel =
    -- First is number of bricks we have, second is direction, left or right
    -- TODO if a number is rolled, decrease the chance that it is rolled again, and higher all others
    -- See random.weighted
    Random.pair (Random.int 1 totalBrickTypes) (Random.int 1 2)


tryGetRandomBrickForm : Int -> Result String BrickForm
tryGetRandomBrickForm number =
    case number of
        1 ->
            Ok squareStartForm

        2 ->
            Ok straightStartForm

        3 ->
            Ok sShapeStartForm

        4 ->
            Ok zShapeStartForm

        5 ->
            Ok lShapeStartForm

        6 ->
            Ok reversedLShapeStartForm

        7 ->
            Ok bShapeStartForm

        8 ->
            Ok pShapeStartForm

        _ ->
            Err ("Wrong number for brick form : " ++ String.fromInt number)


tryGetRandomDirection : Int -> Result String BrickMoveDirection
tryGetRandomDirection number =
    case number of
        1 ->
            Ok Left

        2 ->
            Ok Right

        _ ->
            Err ("Wrong number for direction : " ++ String.fromInt number)
