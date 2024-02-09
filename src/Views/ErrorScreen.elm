module Views.ErrorScreen exposing (..)

import Html exposing (Html, div, text)
import Messages exposing (Msg)


errorScreen : Html Msg
errorScreen =
    div [] [ text "Windowscreen is too small to play this game." ]
