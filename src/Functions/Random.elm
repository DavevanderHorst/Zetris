module Functions.Random exposing (..)

import Functions.BrickForm exposing (BrickForm, lShapeStartForm, squareStartForm, totalBrickTypes)
import Functions.BrickMoveDirection exposing (BrickMoveDirection(..))
import Random


rollRandomBrickModel : Random.Generator ( Int, Int )
rollRandomBrickModel =
    -- Fist is number of bricks we have, second is direction, left or right
    -- TODO if a number is rolled, decrease the chance that it is rolled again, and higher all others
    -- See random.weighted
    Random.pair (Random.int 1 totalBrickTypes) (Random.int 1 2)


tryGetBrickForm : Int -> Result String BrickForm
tryGetBrickForm number =
    case number of
        _ ->
            Ok squareStartForm



--1 ->
--    Ok squareStartForm
--
--2 ->
--    Ok lShapeStartForm
--
--_ ->
--    Err ("Wrong number for brick form : " ++ String.fromInt number)


tryGetRandomDirection : Int -> Result String BrickMoveDirection
tryGetRandomDirection number =
    case number of
        1 ->
            Ok Left

        2 ->
            Ok Right

        _ ->
            Err ("Wrong number for direction : " ++ String.fromInt number)
