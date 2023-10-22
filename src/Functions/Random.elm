module Functions.Random exposing (..)

import Models exposing (BrickForm(..), Direction(..))
import Random


rollRandomBrickModel : Random.Generator ( Int, Int )
rollRandomBrickModel =
    -- Fist is number of bricks we have, second is direction, left or right
    Random.pair (Random.int 1 1) (Random.int 1 2)


tryGetBrickForm : Int -> Result String BrickForm
tryGetBrickForm number =
    case number of
        1 ->
            Ok Square

        _ ->
            Err ("Wrong number for brick form : " ++ String.fromInt number)


tryGetRandomDirection : Int -> Result String Direction
tryGetRandomDirection number =
    case number of
        1 ->
            Ok Left

        2 ->
            Ok Right

        _ ->
            Err ("Wrong number for direction : " ++ String.fromInt number)
