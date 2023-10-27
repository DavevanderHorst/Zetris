module Functions.BrickMoveDirection exposing (..)


type BrickMoveDirection
    = Left
    | Right


switchBrickMoveDirection : BrickMoveDirection -> BrickMoveDirection
switchBrickMoveDirection direction =
    case direction of
        Left ->
            Right

        Right ->
            Left


brickMoveDirectionToString : BrickMoveDirection -> String
brickMoveDirectionToString direction =
    case direction of
        Left ->
            "left"

        Right ->
            "right"
