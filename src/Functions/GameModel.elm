module Functions.GameModel exposing (..)

import Constants.Errors exposing (noRowsToRemoveError, toManyRowsToRemoveError)
import Constants.Score exposing (finishedBrickPoints)
import Functions.GameClock exposing (addGameCommandToBackOfGameClock, addGameCommandToFrontOfGameClock)
import Functions.GameScore exposing (addRowScoreToGameModel)
import Functions.Playfield exposing (canBrickBePlacedInPlayField, changeRowColorInPlayField, dropRowNumberInPlayField, setBrickInPlayField)
import Models exposing (BrickModel, Cell, Color(..), GameCommand, GameModel)


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
        Err (error ++ " -- trySetNewBrickInGameModel, TODO")


emptyGameClockForGameModel : GameModel -> GameModel
emptyGameClockForGameModel gameModel =
    { gameModel | gameClock = [] }


setGameClockInGameModel : List GameCommand -> GameModel -> GameModel
setGameClockInGameModel newGameClock gameModel =
    { gameModel | gameClock = newGameClock }


addGameCommandToFrontOfGameModelClock : GameCommand -> GameModel -> GameModel
addGameCommandToFrontOfGameModelClock command gameModel =
    let
        newGameClock =
            addGameCommandToFrontOfGameClock command gameModel.gameClock
    in
    { gameModel | gameClock = newGameClock }


addGameCommandToBackOfGameModelClock : GameCommand -> GameModel -> GameModel
addGameCommandToBackOfGameModelClock command gameModel =
    let
        newGameClock =
            addGameCommandToBackOfGameClock command gameModel.gameClock
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
                            , score = gameModel.score + finishedBrickPoints
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


removeTempPlayFieldFromGameModel : GameModel -> GameModel
removeTempPlayFieldFromGameModel gameModel =
    { gameModel | tempPlayField = Nothing }


makeRowsWhiteInTempPlayFieldForGameModel : List Int -> GameModel -> GameModel
makeRowsWhiteInTempPlayFieldForGameModel rowNumbers gameModel =
    changeRowColorInTempPlayFieldForGameModel White rowNumbers gameModel


changeRowColorInTempPlayFieldForGameModel : Color -> List Int -> GameModel -> GameModel
changeRowColorInTempPlayFieldForGameModel color rowNumbers gameModel =
    let
        newTempPlayField =
            List.foldl (changeRowColorInPlayField color) gameModel.playField rowNumbers
    in
    { gameModel | tempPlayField = Just newTempPlayField }


makeRowsWhiteInPlayFieldForGameModel : List Int -> GameModel -> GameModel
makeRowsWhiteInPlayFieldForGameModel rowNumbers gameModel =
    let
        newPlayField =
            List.foldl (changeRowColorInPlayField White) gameModel.playField rowNumbers
    in
    { gameModel | playField = newPlayField }


removeRowsFromGameModelAndAdjustScore : Bool -> List Int -> GameModel -> Result String GameModel
removeRowsFromGameModelAndAdjustScore isZetris rows gameModel =
    if List.isEmpty rows then
        Err noRowsToRemoveError

    else
        let
            numberOfRows =
                List.length rows
        in
        if numberOfRows > 4 then
            Err (toManyRowsToRemoveError numberOfRows)

        else
            let
                startRowNumber =
                    Maybe.withDefault 0 (List.minimum rows) - 1
            in
            let
                dropRowResult =
                    dropRowFromGameModelRecursive startRowNumber numberOfRows (Ok gameModel)
            in
            case dropRowResult of
                Err err ->
                    Err err

                Ok newGameModel ->
                    let
                        newGameModelWithScore =
                            addRowScoreToGameModel numberOfRows newGameModel isZetris
                    in
                    Ok (makeRowsWhiteInPlayFieldForGameModel (List.range 1 numberOfRows) newGameModelWithScore)


dropRowFromGameModelRecursive : Int -> Int -> Result String GameModel -> Result String GameModel
dropRowFromGameModelRecursive rowNumber rowsToDrop gameModelResult =
    if rowNumber == 0 then
        gameModelResult

    else
        case gameModelResult of
            Err err ->
                Err err

            Ok gameModel ->
                let
                    newPlayFieldResult =
                        dropRowNumberInPlayField rowNumber rowsToDrop gameModel.playField
                in
                case newPlayFieldResult of
                    Err err ->
                        Err err

                    Ok newPlayField ->
                        dropRowFromGameModelRecursive (rowNumber - 1) rowsToDrop (Ok { gameModel | playField = newPlayField })
