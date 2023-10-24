module Functions.GameClock exposing (..)

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
