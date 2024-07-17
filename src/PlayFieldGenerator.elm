module PlayFieldGenerator exposing (..)

import Constants.PlayFieldSizes exposing (evenRowColumnCells, maximumRows, middleColumnCellNumber, unevenRowColumnCells)
import Dict exposing (Dict)
import Functions.Base exposing (isEven)
import Functions.PlayFieldDictKeys exposing (makePlayFieldDictKey)
import Models exposing (Cell, Color(..), GameModel)


initGameModel : GameModel
initGameModel =
    { playField = createPlayField
    , tempPlayField = Nothing
    , currentBrickModel = Nothing
    , gameClock = []
    , score = 0
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
            --if rowNumber == maximumRows && colNumber < 8 then
            --    Orange
            --
            --else if rowNumber == maximumRows && colNumber > 8 then
            --    Red
            --
            --else if rowNumber == maximumRows - 1 && colNumber < 6 then
            --    Orange
            --
            --else if rowNumber == maximumRows - 1 && colNumber > 8 then
            --    Red
            --
            --else if rowNumber == maximumRows - 2 && colNumber < 7 then
            --    Red
            --
            --else if rowNumber == maximumRows - 2 && colNumber > 8 then
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
