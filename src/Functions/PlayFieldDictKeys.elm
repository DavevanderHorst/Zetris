module Functions.PlayFieldDictKeys exposing (..)


makePlayFieldDictKey : Int -> Int -> String
makePlayFieldDictKey rowNumber colNumber =
    String.fromInt rowNumber ++ "," ++ String.fromInt colNumber


getRowNumberFromPlayFieldDictKey : String -> Int
getRowNumberFromPlayFieldDictKey key =
    -- Cant go wrong.
    let
        rowNumberString =
            case String.split "," key of
                [ row, _ ] ->
                    row

                _ ->
                    ""
    in
    Maybe.withDefault 0 (String.toInt rowNumberString)


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
