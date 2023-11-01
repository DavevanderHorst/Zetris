module Functions.GameModel exposing (..)

import Functions.GameClock exposing (addCommandToBackOfGameClock, addCommandToFrontOfGameClock)
import Functions.Playfield exposing (canBrickBePlacedInPlayField, setBrickInPlayField)
import Models exposing (BrickModel, Cell, GameCommand, GameModel)


trySetNewBrickInGameModel : GameModel -> BrickModel -> Result String GameModel
trySetNewBrickInGameModel gameModel brick =
    let
        ( canBePlaced, error ) =
            canBrickBePlacedInPlayField brick.playFieldDictKeys gameModel.playField
    in
    if canBePlaced then
        let
            newPlayField =
                setBrickInPlayField brick gameModel.playField
        in
        Ok { gameModel | tempPlayField = Just newPlayField, currentBrickModel = Just brick }

    else
        Err (error ++ " TODO")


emptyGameClock : GameModel -> GameModel
emptyGameClock gameModel =
    { gameModel | gameClock = [] }


setGameClockInGameModel : List GameCommand -> GameModel -> GameModel
setGameClockInGameModel newGameClock gameModel =
    { gameModel | gameClock = newGameClock }


addGameCommandToFrontOfGameModelClock : GameCommand -> GameModel -> GameModel
addGameCommandToFrontOfGameModelClock command gameModel =
    let
        newGameClock =
            addCommandToFrontOfGameClock command gameModel.gameClock
    in
    { gameModel | gameClock = newGameClock }


addGameCommandToBackOfGameModelClock : GameCommand -> GameModel -> GameModel
addGameCommandToBackOfGameModelClock command gameModel =
    let
        newGameClock =
            addCommandToBackOfGameClock command gameModel.gameClock
    in
    { gameModel | gameClock = newGameClock }


updateGameModelForFinishedBrick : GameModel -> Result String ( GameModel, BrickModel )
updateGameModelForFinishedBrick gameModel =
    case gameModel.tempPlayField of
        Nothing ->
            Err "No temp play field to update game model for finished brick"

        Just playField ->
            case gameModel.currentBrickModel of
                Nothing ->
                    Err "No current brick model to update game model for finished brick"

                Just brickModel ->
                    let
                        newBrickModel =
                            { brickModel | isActive = False }
                    in
                    Ok
                        ( { gameModel
                            | playField = playField
                            , tempPlayField = Nothing
                            , currentBrickModel = Just newBrickModel
                          }
                        , newBrickModel
                        )


tryTakeActiveBrickModelFromGameModel : GameModel -> Result String BrickModel
tryTakeActiveBrickModelFromGameModel gameModel =
    case gameModel.currentBrickModel of
        Nothing ->
            Err "There is no current brick model"

        Just brickModel ->
            if not brickModel.isActive then
                Err "Current brickModel is not active"

            else
                Ok brickModel
