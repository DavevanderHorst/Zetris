module Models exposing (..)

import Dict exposing (Dict)


type alias Model =
    -- Main model
    { windowSize : Size
    , error : Maybe String
    , gameModel : GameModel
    }


type alias GameModel =
    { playField : Dict String Cell
    , tempPlayField : Maybe (Dict String Cell)
    , currentBrickModel : Maybe BrickModel
    , gameClock : List GameCommand
    }


type alias BrickModel =
    { form : BrickForm
    , direction : Direction
    , baseRow : Int
    , baseColumn : Int
    , playFieldDictKeys : List String
    }


type GameCommand
    = DropBrick


type BrickForm
    = Square


type Direction
    = Left
    | Right


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


type Color
    = White
    | Red
