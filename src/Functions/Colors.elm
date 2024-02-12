module Functions.Colors exposing (..)

import Functions.BrickForm exposing (BrickForm(..))
import Models exposing (Color(..))


cellColorToString : Color -> String
cellColorToString color =
    case color of
        White ->
            "white"

        Violet ->
            "violet"

        Indigo ->
            "indigo"

        Blue ->
            "blue"

        Green ->
            "green"

        Yellow ->
            "yellow"

        Orange ->
            "orange"

        Red ->
            "red"


zetrisAnimationStartColor : Color
zetrisAnimationStartColor =
    White


getNextZetrisAnimationColor : Color -> Color
getNextZetrisAnimationColor color =
    case color of
        White ->
            Violet

        Violet ->
            Indigo

        Indigo ->
            Blue

        Blue ->
            Green

        Green ->
            Yellow

        Yellow ->
            Orange

        Orange ->
            Red

        Red ->
            White


getBrickFormColor : BrickForm -> Color
getBrickFormColor form =
    case form of
        Square _ ->
            Red

        LShape _ ->
            Orange

        Straight _ ->
            Blue

        SShape _ ->
            Yellow

        ZShape _ ->
            Green
