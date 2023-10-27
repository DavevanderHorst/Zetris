module Functions.Colors exposing (..)

import Functions.BrickForm exposing (BrickForm(..))
import Models exposing (Color(..))


cellColorToString : Color -> String
cellColorToString color =
    case color of
        White ->
            "white"

        Red ->
            "red"

        Orange ->
            "orange"


getBrickFormColor : BrickForm -> Color
getBrickFormColor form =
    case form of
        Square _ ->
            Red
