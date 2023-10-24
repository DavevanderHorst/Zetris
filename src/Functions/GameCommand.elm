module Functions.GameCommand exposing (..)

import Functions.Brick exposing (dropBrickModel, isBrickModelAtBottom)
import Functions.GameModel exposing (trySetNewBrickInGameModel)
import Models exposing (GameCommand(..), GameModel)


executeGameCommand : GameCommand -> GameModel -> Result String GameModel
executeGameCommand command gameModel =
    case command of
        DropBrick ->
            case gameModel.currentBrickModel of
                Nothing ->
                    Err "Cant drop brick, because there is none."

                Just brickModel ->
                    if isBrickModelAtBottom brickModel then
                        -- ok bottom, now we need to start new brick
                        Err "TODO"

                    else
                        let
                            nextBrickModel =
                                dropBrickModel brickModel
                        in
                        trySetNewBrickInGameModel gameModel nextBrickModel
