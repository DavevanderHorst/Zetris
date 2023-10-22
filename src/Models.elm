module Models exposing (..)

import Dict exposing (Dict)


type alias Model =
    -- Main model
    { windowSize : Size
    , playField : Dict String Cell
    , tempPlayField : Maybe (Dict String Cell)
    , currentBrickModel : Maybe BrickModel
    , error : Maybe String
    }


type alias BrickModel =
    { currentForm : BrickForm
    , currentDirection : Direction
    }


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
