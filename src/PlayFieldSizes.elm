module PlayFieldSizes exposing (..)


unevenRowColumnCells : Int
unevenRowColumnCells =
    -- also start row, must be uneven, so we have a middle point
    15


middleColumnCellNumber : Int
middleColumnCellNumber =
    Basics.round (toFloat unevenRowColumnCells / 2)


evenRowColumnCells : Int
evenRowColumnCells =
    unevenRowColumnCells - 1


cellsInHeight : Int
cellsInHeight =
    27


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


cellLeftPaddingInPxString : String
cellLeftPaddingInPxString =
    String.fromInt cellLeftPaddingInPx


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


playFieldWidthInPxString : String
playFieldWidthInPxString =
    --background padding + cell padding + cell width
    let
        totalCellWidth =
            unevenRowColumnCells * cellSize

        fieldPadding =
            2 * playFieldBorderPaddingInPx

        cellPadding =
            (unevenRowColumnCells - 1) * cellLeftPaddingInPx
    in
    String.fromInt (totalCellWidth + fieldPadding + cellPadding) ++ "px"


playFieldHeightInPxString : String
playFieldHeightInPxString =
    --background padding + cell padding + 3/4 cell width
    let
        totalCellHeight =
            (cellsInHeight - 1) * threeQuarterCellSize + cellSize

        fieldPadding =
            2 * playFieldBorderPaddingInPx

        cellPadding =
            (cellsInHeight - 1) * cellLeftPaddingInPx
    in
    String.fromInt (totalCellHeight + fieldPadding + cellPadding) ++ "px"
