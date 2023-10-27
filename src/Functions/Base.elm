module Functions.Base exposing (..)


isEven : Int -> Bool
isEven number =
    if modBy 2 number == 0 then
        True

    else
        False


addToBackOfList : a -> List a -> List a
addToBackOfList toAdd list =
    let
        reversedList =
            List.reverse list
    in
    List.reverse <| toAdd :: reversedList


makePlayFieldDictKey : Int -> Int -> String
makePlayFieldDictKey rowNumber colNumber =
    String.fromInt rowNumber ++ "," ++ String.fromInt colNumber
