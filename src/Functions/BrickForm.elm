module Functions.BrickForm exposing (..)


type
    BrickForm
    -- if we add a brick form, then we must add 1 to totalBrickTypes below as well
    = Square ThreeFormType
    | LShape SixFormType
    | ReversedLShape SixFormType
    | SShape TwoFormType
    | ZShape TwoFormType
    | Straight ThreeFormType
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
    LShape F


bShapeStartForm : BrickForm
bShapeStartForm =
    BShape F


pShapeStartForm : BrickForm
pShapeStartForm =
    PShape F


reversedLShapeStartForm : BrickForm
reversedLShapeStartForm =
    ReversedLShape F


sShapeStartForm : BrickForm
sShapeStartForm =
    SShape D


zShapeStartForm : BrickForm
zShapeStartForm =
    ZShape D


straightStartForm : BrickForm
straightStartForm =
    Straight A


type TwoFormType
    = D
    | E


type ThreeFormType
    = A
    | B
    | C


type SixFormType
    = F
    | G
    | H
    | I
    | J
    | K


switchTwoFormType : TwoFormType -> TwoFormType
switchTwoFormType formType =
    case formType of
        D ->
            E

        E ->
            D


switchThreeFormType : ThreeFormType -> ThreeFormType
switchThreeFormType formType =
    case formType of
        A ->
            B

        B ->
            C

        C ->
            A


switchSixFormType : SixFormType -> SixFormType
switchSixFormType formType =
    case formType of
        F ->
            G

        G ->
            H

        H ->
            I

        I ->
            J

        J ->
            K

        K ->
            F
