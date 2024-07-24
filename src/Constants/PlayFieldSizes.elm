module Constants.PlayFieldSizes exposing (..)


rightColumnWidth : Int
rightColumnWidth =
    500


maximumRows : Int
maximumRows =
    21


unevenRowColumnCells : Int
unevenRowColumnCells =
    -- also start row, must be uneven, so we have a middle point
    11


evenRowColumnCells : Int
evenRowColumnCells =
    unevenRowColumnCells - 1


brickStartRowNumber : Int
brickStartRowNumber =
    -- if switched to an uneven number, also adjust  -brickStartRowMiddleColumnCellNumber- just below
    2


brickStartRowMiddleColumnCellNumber : Int
brickStartRowMiddleColumnCellNumber =
    evenRowMiddleColumnCellNumber


evenRowMiddleColumnCellNumber : Int
evenRowMiddleColumnCellNumber =
    Basics.round (toFloat evenRowColumnCells / 2)


cellSize : Int
cellSize =
    -- divisible by 4
    32


threeQuarterCellSize : Int
threeQuarterCellSize =
    cellSize // 4 * 3


cellLeftPaddingInPx : Int
cellLeftPaddingInPx =
    1


playFieldBorderPaddingInPx : Int
playFieldBorderPaddingInPx =
    10


cellSizeInPxString : String
cellSizeInPxString =
    String.fromInt cellSize ++ "px"


playFieldBorderPaddingInPxString : String
playFieldBorderPaddingInPxString =
    String.fromInt playFieldBorderPaddingInPx ++ "px"


negativeUpPaddingInPxString : Int -> String
negativeUpPaddingInPxString rowNumber =
    -- for letting the cells fall into each other
    if rowNumber == 1 then
        "0"

    else
        let
            upPadding =
                (cellSize // 4) - cellLeftPaddingInPx

            multiplier =
                rowNumber - 1
        in
        "-" ++ String.fromInt (upPadding * multiplier) ++ "px"


halfCellWidthInPxString : String
halfCellWidthInPxString =
    String.fromInt (cellSize // 2) ++ "px"


playFieldWidth : Int
playFieldWidth =
    --background padding + cell padding + cell width
    let
        totalCellWidth =
            unevenRowColumnCells * cellSize

        fieldPadding =
            2 * playFieldBorderPaddingInPx

        cellPadding =
            (unevenRowColumnCells - 1) * cellLeftPaddingInPx
    in
    totalCellWidth + fieldPadding + cellPadding


playFieldHeight : Int
playFieldHeight =
    --background padding + cell padding + 3/4 cell width
    let
        totalCellHeight =
            (maximumRows - 1) * threeQuarterCellSize + cellSize

        fieldPadding =
            2 * playFieldBorderPaddingInPx

        cellPadding =
            (maximumRows - 1) * cellLeftPaddingInPx
    in
    totalCellHeight + fieldPadding + cellPadding
