module Constants.Errors exposing (..)


noRowsToRemoveError : String
noRowsToRemoveError =
    "No rows to remove from game model"


toManyRowsToRemoveError : Int -> String
toManyRowsToRemoveError rows =
    "Cant remove more then 4 rows in one time, number of rows to remove = " ++ String.fromInt rows
