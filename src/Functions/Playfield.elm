module Functions.Playfield exposing (..)

import Dict exposing (Dict)
import Models exposing (BrickForm(..), BrickModel, Cell, Color(..))
import PlayFieldSizes exposing (middleColumnCellNumber)


makePlayFieldDictKey : Int -> Int -> String
makePlayFieldDictKey rowNumber colNumber =
    String.fromInt rowNumber ++ "," ++ String.fromInt colNumber


getRowAndColNumberFromPlayFieldDictKey : String -> ( Int, Int )
getRowAndColNumberFromPlayFieldDictKey key =
    -- Cant go wrong.
    let
        ( rowNumberString, colNumberString ) =
            case String.split "," key of
                [ row, col ] ->
                    ( row, col )

                _ ->
                    ( "", "" )
    in
    ( Maybe.withDefault 0 (String.toInt rowNumberString), Maybe.withDefault 0 (String.toInt colNumberString) )


setNewBrickInPlayField : Dict String Cell -> BrickModel -> Dict String Cell
setNewBrickInPlayField playField brickModel =
    case brickModel.currentForm of
        Square ->
            setSquareInPlayField middleColumnCellNumber playField


setSquareInPlayField : Int -> Dict String Cell -> Dict String Cell
setSquareInPlayField startColumnNumber playField =
    let
        firstSet =
            setBrick Square 1 startColumnNumber playField

        secondSet =
            setBrick Square 2 startColumnNumber firstSet

        thirdSet =
            setBrick Square 2 (startColumnNumber - 1) secondSet
    in
    setBrick Square 3 startColumnNumber thirdSet


setBrick : BrickForm -> Int -> Int -> Dict String Cell -> Dict String Cell
setBrick form row col playField =
    let
        key =
            makePlayFieldDictKey row col

        color =
            getBrickFormColor form
    in
    Dict.insert key (Cell color) playField


getBrickFormColor : BrickForm -> Color
getBrickFormColor form =
    case form of
        Square ->
            Red
