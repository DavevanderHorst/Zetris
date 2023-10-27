module Functions.GameClock exposing (..)

import Functions.Base exposing (addToBackOfList)
import Models exposing (GameCommand)


tickGameClock : List GameCommand -> ( Maybe GameCommand, List GameCommand )
tickGameClock gameClock =
    let
        maybeGameCommand =
            List.head gameClock

        newGameClock =
            List.drop 1 gameClock
    in
    ( maybeGameCommand, newGameClock )


addCommandToBackOfGameClock : GameCommand -> List GameCommand -> List GameCommand
addCommandToBackOfGameClock command gameClock =
    addToBackOfList command gameClock


addCommandToFrontOfGameClock : GameCommand -> List GameCommand -> List GameCommand
addCommandToFrontOfGameClock command gameClock =
    command :: gameClock
