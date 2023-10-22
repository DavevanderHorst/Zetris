module PlayFieldGenerator exposing (..)

import Dict exposing (Dict)
import Functions.Base exposing (isEven)
import Functions.Playfield exposing (makePlayFieldDictKey)
import Models exposing (Cell, Color(..))
import PlayFieldSizes exposing (cellsInHeight, evenRowColumnCells, unevenRowColumnCells)


createPlayField : Dict String Cell
createPlayField =
    List.foldl generatePlayFieldRows Dict.empty (List.range 1 cellsInHeight)


generatePlayFieldRows : Int -> Dict String Cell -> Dict String Cell
generatePlayFieldRows rowNumber dict =
    let
        columns =
            if isEven rowNumber then
                evenRowColumnCells

            else
                unevenRowColumnCells
    in
    List.foldl (generatePlayFieldRow rowNumber) dict (List.range 1 columns)


generatePlayFieldRow : Int -> Int -> Dict String Cell -> Dict String Cell
generatePlayFieldRow rowNumber colNumber dict =
    let
        key =
            makePlayFieldDictKey rowNumber colNumber
    in
    Dict.insert key (Cell White) dict
