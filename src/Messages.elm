module Messages exposing (..)

import Browser.Dom exposing (Viewport)


type Msg
    = GotViewport Viewport
    | KeyPressed String
    | StartGame
    | StartClock
    | Tick
    | NewBrick ( Int, Int )
    | DropCurrentBrick
    | ActivatePlayerInput


type KeyValue
    = Character Char
    | Control String
