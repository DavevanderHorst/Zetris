module Functions.GameModel exposing (..)

import Functions.Playfield exposing (canBrickBePlacedInPlayField, setBrickInPlayField)
import Models exposing (BrickModel, GameModel)


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
        -- todo cant be place so next block
        Err (error ++ " TODO")
