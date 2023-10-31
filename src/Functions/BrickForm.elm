module Functions.BrickForm exposing (..)


type
    BrickForm
    -- if we add a brick form, then we must add 1 to totalBrickTypes as well
    = Square ThreeFormType
    | LShape SixFormType


totalBrickTypes : Int
totalBrickTypes =
    2


squareStartForm : BrickForm
squareStartForm =
    Square A


lShapeStartForm : BrickForm
lShapeStartForm =
    LShape Aa


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
