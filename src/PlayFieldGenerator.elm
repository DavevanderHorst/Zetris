module PlayFieldGenerator exposing (..)

import Dict exposing (Dict)
import Functions.Base exposing (isEven)
import Functions.PlayFieldDictKeys exposing (makePlayFieldDictKey)
import Models exposing (Cell, Color(..), GameModel)
import PlayFieldSizes exposing (evenRowColumnCells, maximumRows, middleColumnCellNumber, unevenRowColumnCells)


initGameModel : GameModel
initGameModel =
    { playField = createPlayField
    , tempPlayField = Nothing
    , currentBrickModel = Nothing
    , gameClock = []
    }


createPlayField : Dict String Cell
createPlayField =
    List.foldl generatePlayFieldRows Dict.empty (List.range 1 maximumRows)


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

        color =
            --if rowNumber == maximumRows then
            --    if colNumber == middleColumnCellNumber then
            --        White
            --
            --    else
            --        Red
            --
            --else if rowNumber == maximumRows - 1 && colNumber < 5 then
            --    Red
            --
            --else if rowNumber == maximumRows - 2 && colNumber < 5 then
            --    Red
            --
            --else if rowNumber == maximumRows - 3 && colNumber < 5 then
            --    Red
            --
            --else if rowNumber == maximumRows - 4 && colNumber > 10 then
            --    Red
            --
            --else
            White
    in
    Dict.insert key (Cell color) dict
