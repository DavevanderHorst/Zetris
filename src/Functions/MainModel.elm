module Functions.MainModel exposing (..)

import Functions.GameModel exposing (setGameClockInGameModel)
import Models exposing (GameCommand, MainModel)


setGameClockInMainModel : List GameCommand -> MainModel -> MainModel
setGameClockInMainModel gameClock model =
    let
        newGameModel =
            setGameClockInGameModel gameClock model.gameModel
    in
    { model | gameModel = newGameModel }
