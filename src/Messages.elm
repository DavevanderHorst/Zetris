module Messages exposing (..)

import Browser.Dom exposing (Viewport)
import Models exposing (Color, GameCommand)


type Msg
    = GotViewport Viewport
    | GotNewSize Int Int
    | KeyPressed String
    | StartGame
    | StartClock
    | Tick
    | NewBrick ( Int, Int )
    | MakeNewBrick
    | DropCurrentBrick
    | ActivatePlayerInput
    | FullRowAnimation Int (List Int)
    | ZetrisAnimation Color Int (List Int)
    | ExecuteGameCommand GameCommand
