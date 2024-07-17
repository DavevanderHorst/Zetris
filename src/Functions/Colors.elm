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
            "#0093ff"

        DarkBlue ->
            "#1b00ff"

        Green ->
            "#8fff00"

        DarkGreen ->
            "#1e8449"

        Yellow ->
            "#ffd500"

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
            DarkBlue

        DarkBlue ->
            Green

        Green ->
            DarkGreen

        DarkGreen ->
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

        ReversedLShape _ ->
            Indigo

        Straight _ ->
            Blue

        SShape _ ->
            Yellow

        ZShape _ ->
            Green

        BShape _ ->
            DarkBlue

        PShape _ ->
            DarkGreen
