module Functions.GameCommand exposing (..)

import Constants.Score exposing (dropDownByPlayerPoints)
import Constants.Timers exposing (newBrickWaitTime, numberOfRowBlinks)
import Functions.Brick exposing (dropBrickModel, moveBrickModelLeft, moveBrickModelRight, switchBrickDropDirection, switchBrickForm)
import Functions.GameModel exposing (setBrickInGameModel, trySetNewBrickInGameModel, tryTakeActiveBrickModelFromGameModel, updateGameModelForFinishedBrick)
import Functions.Playfield exposing (getFullRowsAfterSettingBrick)
import Messages exposing (Msg(..))
import Models exposing (BrickModel, GameCommand(..), GameModel, MainModel)
import Process
import Task


executeGameCommand : GameCommand -> MainModel -> ( MainModel, Cmd Msg )
executeGameCommand command model =
    let
        takeActiveBrickResult =
            tryTakeActiveBrickModelFromGameModel model.gameModel
    in
    case takeActiveBrickResult of
        Err error ->
            ( { model | error = Just error }, Cmd.none )

        Ok brickModel ->
            case command of
                DropBrick ->
                    dropBrickCommand False brickModel model

                DropBrickByPlayer ->
                    dropBrickCommand True brickModel model

                MoveLeft ->
                    let
                        nextBrickModel =
                            moveBrickModelLeft brickModel
                    in
                    case trySetNewBrickInGameModel model.gameModel nextBrickModel of
                        Err error ->
                            -- todo , sound animation or anything to show false move
                            -- brick hitting other bricks/wall, so cant move
                            ( { model | error = Just error }, Cmd.none )

                        Ok newGameModel ->
                            -- brick moved left
                            ( { model | gameModel = newGameModel }, Cmd.none )

                MoveRight ->
                    let
                        nextBrickModel =
                            moveBrickModelRight brickModel
                    in
                    case trySetNewBrickInGameModel model.gameModel nextBrickModel of
                        Err error ->
                            -- todo , sound animation or anything to show false move
                            -- brick hitting other bricks/walls, so cant move.
                            ( { model | error = Just error }, Cmd.none )

                        Ok newGameModel ->
                            -- brick moved right
                            ( { model | gameModel = newGameModel }, Cmd.none )

                SwitchForm switchDirection ->
                    let
                        nextBrickModel =
                            switchBrickForm switchDirection brickModel
                    in
                    case trySetNewBrickInGameModel model.gameModel nextBrickModel of
                        Err error ->
                            -- todo , sound animation or anything to show false form switch
                            -- brick hitting other bricks/walls, so cant change form.
                            ( { model | error = Just error }, Cmd.none )

                        Ok newGameModel ->
                            -- brick switched forms
                            ( { model | gameModel = newGameModel }, Cmd.none )

                SwitchDropDirection ->
                    let
                        nextBrickModel =
                            switchBrickDropDirection brickModel
                    in
                    let
                        newGameModel =
                            setBrickInGameModel model.gameModel nextBrickModel
                    in
                    ( { model | gameModel = newGameModel }, Cmd.none )


dropBrickCommand : Bool -> BrickModel -> MainModel -> ( MainModel, Cmd Msg )
dropBrickCommand byPlayer brickModel model =
    let
        nextBrickModel =
            dropBrickModel brickModel

        oldGameModel =
            model.gameModel

        newGameModel =
            if byPlayer then
                { oldGameModel
                    | score = oldGameModel.score + dropDownByPlayerPoints
                }

            else
                oldGameModel
    in
    case trySetNewBrickInGameModel newGameModel nextBrickModel of
        Err err ->
            -- brick hitting other bricks or bottom, so sticks.
            let
                finishedGameModelResult =
                    updateGameModelForFinishedBrick newGameModel
            in
            case finishedGameModelResult of
                Err error ->
                    ( { model | error = Just (error ++ ", " ++ err) }, Cmd.none )

                Ok ( finishedGameModel, updatedBrickModel ) ->
                    let
                        fullRowsList =
                            getFullRowsAfterSettingBrick finishedGameModel.playField updatedBrickModel
                    in
                    if List.isEmpty fullRowsList then
                        ( { model | gameModel = finishedGameModel, error = Just err }
                        , Process.sleep newBrickWaitTime |> Task.perform (always MakeNewBrick)
                        )

                    else
                        -- full rows, we start full row animation, after the animation, we remove
                        ( { model | gameModel = finishedGameModel }
                        , Task.perform (\_ -> FullRowAnimation numberOfRowBlinks fullRowsList) (Task.succeed True)
                        )

        Ok newBrickInGameModel ->
            -- brick dropped
            ( { model | gameModel = newBrickInGameModel }, Cmd.none )
