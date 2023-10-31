module Functions.Playfield exposing (..)

import Dict exposing (Dict)
import Functions.BrickForm exposing (BrickForm(..))
import Functions.Colors exposing (cellColorToString, getBrickFormColor)
import Functions.Shapes.LShape exposing (createLShapePlayFieldDictKeys)
import Functions.Shapes.Square exposing (createSquarePlayFieldDictKeys)
import Models exposing (BrickModel, Cell, Color(..), GameModel, MainModel)


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


createPlayFieldDictKeysForBrickForm : Int -> Int -> BrickForm -> List String
createPlayFieldDictKeysForBrickForm row col form =
    case form of
        Square formType ->
            createSquarePlayFieldDictKeys row col formType

        LShape formType ->
            createLShapePlayFieldDictKeys row col formType


setBrickInPlayField : BrickModel -> Dict String Cell -> Dict String Cell
setBrickInPlayField brick playField =
    let
        color =
            getBrickFormColor brick.form
    in
    List.foldl (setCellInPlayField color) playField brick.playFieldDictKeys


setCellInPlayField : Color -> String -> Dict String Cell -> Dict String Cell
setCellInPlayField color key playField =
    Dict.insert key (Cell color) playField


canBrickBePlacedInPlayField : List String -> Dict String Cell -> ( Bool, String )
canBrickBePlacedInPlayField keys playField =
    List.foldl (isCellEmpty playField) ( True, "" ) keys


isCellEmpty : Dict String Cell -> String -> ( Bool, String ) -> ( Bool, String )
isCellEmpty dict key result =
    let
        ( isOk, _ ) =
            result
    in
    if isOk then
        let
            cellResult =
                Dict.get key dict
        in
        case cellResult of
            Nothing ->
                ( False, "No cell found for key : " ++ key )

            Just cell ->
                if cell.color == White then
                    result

                else
                    ( False, "Cell has wrong color : " ++ cellColorToString cell.color )

    else
        result
