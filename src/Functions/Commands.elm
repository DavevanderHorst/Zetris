module Functions.Commands exposing (..)

import Constants.Timers exposing (brickDropTime, gameClockWaitTime, rowBlinkTime, zetrisBlinkTime)
import Functions.Colors exposing (getNextZetrisAnimationColor)
import Functions.Random exposing (rollRandomBrickModel)
import Messages exposing (Msg(..))
import Models exposing (Color)
import Process
import Random
import Task


nextTickCmd : Cmd Msg
nextTickCmd =
    Process.sleep gameClockWaitTime |> Task.perform (always Tick)


newBrickCommand : Cmd Msg
newBrickCommand =
    Random.generate NewBrick rollRandomBrickModel


fallingBrickCommand : Cmd Msg
fallingBrickCommand =
    Process.sleep brickDropTime |> Task.perform (always DropCurrentBrick)


makeNextZetrisCommand : Color -> Int -> List Int -> Cmd Msg
makeNextZetrisCommand color blinkNumber fullRowNumbers =
    let
        nextBlinkNumber =
            blinkNumber - 1

        nextColor =
            getNextZetrisAnimationColor color
    in
    Process.sleep zetrisBlinkTime |> Task.perform (always (ZetrisAnimation nextColor nextBlinkNumber fullRowNumbers))


makeNextFullRowsCommand : Int -> List Int -> Cmd Msg
makeNextFullRowsCommand blinkNumber fullRowNumbers =
    let
        nextBlinkNumber =
            blinkNumber - 1
    in
    Process.sleep rowBlinkTime |> Task.perform (always (FullRowAnimation nextBlinkNumber fullRowNumbers))
