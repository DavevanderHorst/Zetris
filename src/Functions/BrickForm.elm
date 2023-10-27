module Functions.BrickForm exposing (..)


type
    BrickForm
    -- if we add a brick form, then we must add 1 to totalBrickTypes as well
    = Square FormType


totalBrickTypes : Int
totalBrickTypes =
    1


squareStartForm : BrickForm
squareStartForm =
    Square A


type FormType
    = A
    | B
    | C


switchFormType : FormType -> FormType
switchFormType formType =
    case formType of
        A ->
            B

        B ->
            C

        C ->
            A
