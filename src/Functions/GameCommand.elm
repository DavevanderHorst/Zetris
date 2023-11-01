module Functions.GameCommand exposing (..)

import Functions.Brick exposing (dropBrickModel, moveBrickModelLeft, moveBrickModelRight, switchBrickForm)
import Functions.GameModel exposing (trySetNewBrickInGameModel, tryTakeActiveBrickModelFromGameModel, updateGameModelForFinishedBrick)
import Functions.Playfield exposing (getFullRowsAfterSettingBrick)
import Messages exposing (Msg(..))
import Models exposing (GameCommand(..), GameModel, MainModel)


executeGameCommand : GameCommand -> MainModel -> Cmd Msg -> ( MainModel, Cmd Msg )
executeGameCommand command model nextTickCmd =
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
                    let
                        nextBrickModel =
                            dropBrickModel brickModel
                    in
                    case trySetNewBrickInGameModel model.gameModel nextBrickModel of
                        Err err ->
                            -- brick hitting other bricks or bottom, so sticks.
                            let
                                newGameModelResult =
                                    updateGameModelForFinishedBrick model.gameModel
                            in
                            case newGameModelResult of
                                Err error ->
                                    ( { model | error = Just (error ++ ", " ++ err) }, nextTickCmd )

                                Ok ( newGameModel, updatedBrickModel ) ->
                                    let
                                        fullRowsList =
                                            getFullRowsAfterSettingBrick newGameModel.playField updatedBrickModel
                                    in
                                    if List.isEmpty fullRowsList then
                                        ( { model | gameModel = newGameModel, error = Just err }, Cmd.batch [ nextTickCmd ] )

                                    else
                                        ( { model | gameModel = newGameModel, error = Just ("Found full rows!!!!, " ++ err) }, nextTickCmd )

                        Ok newGameModel ->
                            -- brick dropped
                            ( { model | gameModel = newGameModel }, nextTickCmd )

                MoveLeft ->
                    let
                        nextBrickModel =
                            moveBrickModelLeft brickModel
                    in
                    case trySetNewBrickInGameModel model.gameModel nextBrickModel of
                        Err error ->
                            -- todo , sound animation or anything to show false move
                            -- brick hitting other bricks/wall, so cant move
                            ( { model | error = Just error }, nextTickCmd )

                        Ok newGameModel ->
                            -- brick moved left
                            ( { model | gameModel = newGameModel }, nextTickCmd )

                MoveRight ->
                    let
                        nextBrickModel =
                            moveBrickModelRight brickModel
                    in
                    case trySetNewBrickInGameModel model.gameModel nextBrickModel of
                        Err error ->
                            -- todo , sound animation or anything to show false move
                            -- brick hitting other bricks/walls, so cant move.
                            ( { model | error = Just error }, nextTickCmd )

                        Ok newGameModel ->
                            -- brick moved right
                            ( { model | gameModel = newGameModel }, nextTickCmd )

                SwitchForm ->
                    let
                        nextBrickModel =
                            switchBrickForm brickModel
                    in
                    case trySetNewBrickInGameModel model.gameModel nextBrickModel of
                        Err error ->
                            -- todo , sound animation or anything to show false form switch
                            -- brick hitting other bricks/walls, so cant change form.
                            ( { model | error = Just error }, nextTickCmd )

                        Ok newGameModel ->
                            -- brick switched forms
                            ( { model | gameModel = newGameModel }, nextTickCmd )
