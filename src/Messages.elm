module Messages exposing (..)

import Browser.Dom exposing (Viewport)


type Msg
    = GotViewport Viewport
    | StartGame
    | NewBrick ( Int, Int )
