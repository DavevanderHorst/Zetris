module Functions.Commands exposing (..)

import Functions.Random exposing (rollRandomBrickModel)
import Messages exposing (Msg(..))
import Process
import Random
import Task
import Timers exposing (brickDropTime, gameClockWaitTime, rowBlinkTime)


nextTickCmd : Cmd Msg
nextTickCmd =
    Process.sleep gameClockWaitTime |> Task.perform (always Tick)


newBrickCommand : Cmd Msg
newBrickCommand =
    Random.generate NewBrick rollRandomBrickModel


fallingBrickCommand : Cmd Msg
fallingBrickCommand =
    Process.sleep brickDropTime |> Task.perform (always DropCurrentBrick)


nextFullRowsCommand : Int -> List Int -> Cmd Msg
nextFullRowsCommand number fullRowNumbers =
    Process.sleep rowBlinkTime |> Task.perform (always (FullRowAnimation (number - 1) fullRowNumbers))
