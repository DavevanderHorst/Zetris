module Models exposing (..)

import Dict exposing (Dict)
import Functions.BrickForm exposing (BrickForm)
import Functions.BrickMoveDirection exposing (BrickMoveDirection)


type alias MainModel =
    { windowSize : Size
    , error : Maybe String
    , gameModel : GameModel
    , playerInput : PlayerInput
    }


type alias GameModel =
    { playField : Dict String Cell
    , tempPlayField : Maybe (Dict String Cell)
    , currentBrickModel : Maybe BrickModel
    , gameClock : List GameCommand
    , score : Int
    }


type alias BrickModel =
    { form : BrickForm
    , direction : BrickMoveDirection
    , baseRow : Int
    , baseColumn : Int
    , playFieldDictKeys : List String
    , isActive : Bool
    }


type PlayerInput
    = Stopped
    | Possible


type GameCommand
    = DropBrick
    | DropBrickByPlayer
    | MoveLeft
    | MoveRight
    | SwitchForm SwitchDirection
    | SwitchDropDirection


type SwitchDirection
    = ClockWise
    | CounterClockWise


type Color
    = White
    | Violet
    | Indigo
    | Blue
    | DarkBlue
    | Green
    | DarkGreen
    | Yellow
    | Red
    | Orange


type alias Cell =
    { color : Color }


type alias Coordinate =
    { rowNumber : Int
    , columnNumber : Int
    }


type alias Size =
    { width : Float
    , height : Float
    }


startSize : Size
startSize =
    Size 0 0
