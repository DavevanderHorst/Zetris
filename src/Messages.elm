module Messages exposing (..)

import Browser.Dom exposing (Viewport)


type Msg
    = GotViewport Viewport
    | KeyPressed String
    | StartGame
    | StartClock
    | Tick
    | NewBrick ( Int, Int )
    | MakeNewBrick
    | DropCurrentBrick
    | ActivatePlayerInput
    | FullRowAnimation Int (List Int)


type KeyValue
    = Character Char
    | Control String
