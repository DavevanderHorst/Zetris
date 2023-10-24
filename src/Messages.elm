module Messages exposing (..)

import Browser.Dom exposing (Viewport)


type Msg
    = GotViewport Viewport
    | StartGame
    | StartClock
    | Tick
    | NewBrick ( Int, Int )
    | DropCurrentBrick
