module Functions.GameScore exposing (..)

import Constants.Score exposing (fourRowPoints, oneRowPoints, threeRowPoints, twoRowPoints, zetrisRowPoints)
import Models exposing (GameModel)


addRowScoreToGameModel : Int -> GameModel -> Bool -> GameModel
addRowScoreToGameModel numberOfRows gameModel isZetris =
    let
        pointsToAdd =
            if isZetris then
                numberOfRows * zetrisRowPoints

            else
                case numberOfRows of
                    1 ->
                        oneRowPoints

                    2 ->
                        twoRowPoints

                    3 ->
                        threeRowPoints

                    4 ->
                        fourRowPoints

                    _ ->
                        0
    in
    { gameModel | score = gameModel.score + pointsToAdd }
