module Functions.BrickForm exposing (..)

import Constants.PlayFieldSizes exposing (brickStartRowMiddleColumnCellNumber)


type
    BrickForm
    -- if we add a brick form, then we must add 1 to totalBrickTypes below as well
    = Square SixFormType
    | LShape SixFormType
    | ReversedLShape SixFormType
    | SShape SixFormType
    | ZShape SixFormType
    | Straight SixFormType
    | BShape SixFormType
    | PShape SixFormType


totalBrickTypes : Int
totalBrickTypes =
    8


squareStartForm : BrickForm
squareStartForm =
    Square A


lShapeStartForm : BrickForm
lShapeStartForm =
    LShape A


bShapeStartForm : BrickForm
bShapeStartForm =
    BShape A


pShapeStartForm : BrickForm
pShapeStartForm =
    PShape A


reversedLShapeStartForm : BrickForm
reversedLShapeStartForm =
    ReversedLShape A


sShapeStartForm : BrickForm
sShapeStartForm =
    SShape A


zShapeStartForm : BrickForm
zShapeStartForm =
    ZShape A


straightStartForm : BrickForm
straightStartForm =
    Straight B


type SixFormType
    = A
    | B
    | C
    | D
    | E
    | F


getStartColumnNumberForBrickForm : BrickForm -> Int
getStartColumnNumberForBrickForm brickForm =
    case brickForm of
        Square _ ->
            brickStartRowMiddleColumnCellNumber

        LShape _ ->
            brickStartRowMiddleColumnCellNumber

        ReversedLShape _ ->
            brickStartRowMiddleColumnCellNumber

        SShape _ ->
            brickStartRowMiddleColumnCellNumber

        ZShape _ ->
            brickStartRowMiddleColumnCellNumber + 1

        Straight _ ->
            brickStartRowMiddleColumnCellNumber

        BShape _ ->
            brickStartRowMiddleColumnCellNumber

        PShape _ ->
            brickStartRowMiddleColumnCellNumber


switchSixFormTypeRight : SixFormType -> SixFormType
switchSixFormTypeRight formType =
    case formType of
        A ->
            B

        B ->
            C

        C ->
            D

        D ->
            E

        E ->
            F

        F ->
            A


switchSixFormTypeLeft : SixFormType -> SixFormType
switchSixFormTypeLeft formType =
    case formType of
        A ->
            F

        B ->
            A

        C ->
            B

        D ->
            C

        E ->
            D

        F ->
            E
