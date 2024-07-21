module Functions.GameClock exposing (..)

import Functions.Base exposing (addToBackOfList)
import Models exposing (GameCommand)


tickGameClock : List GameCommand -> ( Maybe GameCommand, List GameCommand )
tickGameClock gameClock =
    -- this function takes the first game command and removes it from the game command list, which is returned also
    let
        maybeGameCommand =
            List.head gameClock

        newGameClock =
            List.drop 1 gameClock
    in
    ( maybeGameCommand, newGameClock )


addGameCommandToBackOfGameClock : GameCommand -> List GameCommand -> List GameCommand
addGameCommandToBackOfGameClock command gameClock =
    addToBackOfList command gameClock


addGameCommandToFrontOfGameClock : GameCommand -> List GameCommand -> List GameCommand
addGameCommandToFrontOfGameClock command gameClock =
    command :: gameClock
