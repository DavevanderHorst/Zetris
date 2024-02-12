module Functions.BrickForm exposing (..)


type
    BrickForm
    -- if we add a brick form, then we must add 1 to totalBrickTypes as well
    = Square ThreeFormType
    | LShape SixFormType
    | SShape TwoFormType
    | ZShape TwoFormType
    | Straight ThreeFormType


totalBrickTypes : Int
totalBrickTypes =
    5


squareStartForm : BrickForm
squareStartForm =
    Square A


lShapeStartForm : BrickForm
lShapeStartForm =
    LShape Aa


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
    = Aa
    | Bb
    | Cc
    | Dd
    | Ee
    | Ff


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
        Aa ->
            Bb

        Bb ->
            Cc

        Cc ->
            Dd

        Dd ->
            Ee

        Ee ->
            Ff

        Ff ->
            Aa
