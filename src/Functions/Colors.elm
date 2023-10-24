module Functions.Colors exposing (..)

import Models exposing (BrickForm(..), Color(..))


getBrickFormColor : BrickForm -> Color
getBrickFormColor form =
    case form of
        Square ->
            Red


cellColorToString : Color -> String
cellColorToString color =
    case color of
        White ->
            "white"

        Red ->
            "red"
