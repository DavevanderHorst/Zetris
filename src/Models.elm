module Models exposing (..)

import Dict exposing (Dict)
import Functions.BrickForm exposing (BrickForm, ThreeFormType)
import Functions.BrickMoveDirection exposing (BrickMoveDirection)


type alias MainModel =
    -- Main model
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
    | MoveLeft
    | MoveRight
    | MoveDown
    | SwitchForm


type Color
    = White
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
