module PlayFieldSizes exposing (..)


maximumRows : Int
maximumRows =
    27


unevenRowColumnCells : Int
unevenRowColumnCells =
    -- also start row, must be uneven, so we have a middle point
    15


evenRowColumnCells : Int
evenRowColumnCells =
    unevenRowColumnCells - 1


middleColumnCellNumber : Int
middleColumnCellNumber =
    7



--Basics.round (toFloat unevenRowColumnCells / 2)


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
            (maximumRows - 1) * threeQuarterCellSize + cellSize

        fieldPadding =
            2 * playFieldBorderPaddingInPx

        cellPadding =
            (maximumRows - 1) * cellLeftPaddingInPx
    in
    String.fromInt (totalCellHeight + fieldPadding + cellPadding) ++ "px"
