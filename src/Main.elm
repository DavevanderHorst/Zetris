module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events exposing (onResize)
import Constants.PlayFieldSizes exposing (middleColumnCellNumber)
import Constants.Timers exposing (activatePlayerInPutWaitTime, zetrisRowBlinks)
import Functions.Base exposing (isEven)
import Functions.Brick exposing (createPlayFieldDictKeysForBrickForm, getStartRowNumberForBrickForm, isBrickActive, isThereCurrentActiveBrick)
import Functions.Colors exposing (zetrisAnimationStartColor)
import Functions.Commands exposing (fallingBrickCommand, makeNextFullRowsCommand, makeNextZetrisCommand, newBrickCommand, nextTickCmd)
import Functions.GameClock exposing (tickGameClock)
import Functions.GameCommand exposing (executeGameCommand)
import Functions.GameModel exposing (addGameCommandToBackOfGameModelClock, addGameCommandToFrontOfGameModelClock, changeRowColorInTempPlayFieldForGameModel, emptyGameClock, makeRowsWhiteInTempPlayFieldForGameModel, removeRowsFromGameModelAndAdjustScore, removeTempPlayFieldFromGameModel, trySetNewBrickInGameModel)
import Functions.MainModel exposing (setGameClockInMainModel)
import Functions.Playfield exposing (checkForZetris)
import Functions.Random exposing (tryGetRandomBrickForm, tryGetRandomDirection)
import Json.Decode as Decode
import Messages exposing (Msg(..))
import Models exposing (BrickModel, Cell, GameCommand(..), MainModel, PlayerInput(..), Size, startSize)
import PlayFieldGenerator exposing (initGameModel)
import Process
import Task
import Views.MainView exposing (mainView)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = mainView
        }


subscriptions : MainModel -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown keyDecoder
        , onResize (\w h -> GotNewSize w h)
        ]


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map KeyPressed (Decode.field "key" Decode.string)


init : () -> ( MainModel, Cmd Msg )
init _ =
    ( { windowSize = startSize
      , gameModel = initGameModel
      , error = Nothing
      , playerInput = Stopped
      }
    , Task.perform GotViewport Browser.Dom.getViewport
    )


update : Msg -> MainModel -> ( MainModel, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewPort ->
            handleScreenSize viewPort.viewport.width viewPort.viewport.height model

        GotNewSize width height ->
            handleScreenSize (toFloat width) (toFloat height) model

        KeyPressed key ->
            handleKeyPressed key model

        StartGame ->
            ( model
            , Cmd.batch
                [ newBrickCommand
                , Task.perform (\_ -> StartClock) (Task.succeed True)
                ]
            )

        StartClock ->
            update Tick model

        ActivatePlayerInput ->
            ( { model | playerInput = Possible }, Cmd.none )

        Tick ->
            let
                ( nextMaybeGameCommand, newGameClock ) =
                    tickGameClock model.gameModel.gameClock
            in
            case nextMaybeGameCommand of
                Nothing ->
                    ( model, nextTickCmd )

                Just nextCommand ->
                    let
                        newModel =
                            setGameClockInMainModel newGameClock model
                    in
                    executeGameCommand nextCommand newModel

        MakeNewBrick ->
            if isBrickActive model.gameModel.currentBrickModel then
                ( { model | error = Just "Cant make a new Brick, there is still an active brick" }, Cmd.none )

            else
                ( model, newBrickCommand )

        NewBrick ( randomBrick, randomDirection ) ->
            case tryMakeBrickModel randomBrick randomDirection of
                Ok newBrickModel ->
                    let
                        -- todo new brick can fail, when game over
                        newGameModelResult =
                            trySetNewBrickInGameModel model.gameModel newBrickModel
                    in
                    case newGameModelResult of
                        Ok newGameModel ->
                            let
                                finishedGameModel =
                                    emptyGameClock newGameModel
                            in
                            ( { model | gameModel = finishedGameModel, playerInput = Possible }, fallingBrickCommand )

                        Err error ->
                            ( { model | error = Just error }, Cmd.none )

                Err error ->
                    ( { model | error = Just error }, Cmd.none )

        DropCurrentBrick ->
            if isBrickActive model.gameModel.currentBrickModel then
                let
                    newGameModel =
                        addGameCommandToFrontOfGameModelClock DropBrick model.gameModel
                in
                ( { model | gameModel = newGameModel }, fallingBrickCommand )

            else
                ( { model | error = Just "No active brick" }, Cmd.none )

        ZetrisAnimation color blinks fullRowNumbers ->
            if blinks == 0 then
                -- animation is finished, now we remove the lines
                let
                    newGameModelResult =
                        removeRowsFromGameModelAndAdjustScore True fullRowNumbers model.gameModel
                in
                case newGameModelResult of
                    Err err ->
                        ( { model | error = Just err }, Cmd.none )

                    Ok newGameModel ->
                        -- we continue the game
                        let
                            readyGameModel =
                                removeTempPlayFieldFromGameModel newGameModel
                        in
                        ( { model | gameModel = readyGameModel }
                        , Cmd.batch
                            [ nextTickCmd
                            , Task.perform (\_ -> MakeNewBrick) (Task.succeed True)
                            ]
                        )

            else
                let
                    newGameModel =
                        changeRowColorInTempPlayFieldForGameModel color fullRowNumbers model.gameModel
                in
                ( { model | gameModel = newGameModel }
                , makeNextZetrisCommand color blinks fullRowNumbers
                )

        FullRowAnimation blinks fullRowNumbers ->
            if blinks == 0 then
                -- animation is finished, now we remove and check for ZETRIS
                let
                    newGameModelResult =
                        removeRowsFromGameModelAndAdjustScore False fullRowNumbers model.gameModel
                in
                case newGameModelResult of
                    Err err ->
                        ( { model | error = Just err }, Cmd.none )

                    Ok newGameModel ->
                        -- after removing the full rows, we check if we have ZETRIS.
                        let
                            readyGameModel =
                                removeTempPlayFieldFromGameModel newGameModel

                            newModel =
                                { model | gameModel = readyGameModel }

                            fullRowsList =
                                checkForZetris readyGameModel.playField fullRowNumbers
                        in
                        if List.isEmpty fullRowsList then
                            ( newModel
                            , Cmd.batch
                                [ nextTickCmd
                                , Task.perform (\_ -> MakeNewBrick) (Task.succeed True)
                                ]
                            )

                        else
                            --Start Zetris animation
                            -- full rows, we start full row animation, after the animation, we remove
                            ( newModel
                            , Task.perform (\_ -> ZetrisAnimation zetrisAnimationStartColor zetrisRowBlinks fullRowsList) (Task.succeed True)
                            )

            else
                let
                    newGameModel =
                        if isEven blinks then
                            makeRowsWhiteInTempPlayFieldForGameModel fullRowNumbers model.gameModel

                        else
                            removeTempPlayFieldFromGameModel model.gameModel
                in
                ( { model | gameModel = newGameModel }
                , makeNextFullRowsCommand blinks fullRowNumbers
                )


tryMakeBrickModel : Int -> Int -> Result String BrickModel
tryMakeBrickModel randomBrickForm randomDirection =
    let
        brickFormResult =
            tryGetRandomBrickForm randomBrickForm

        directionResult =
            tryGetRandomDirection randomDirection
    in
    case brickFormResult of
        Ok brickForm ->
            case directionResult of
                Ok direction ->
                    let
                        startRowNumber =
                            getStartRowNumberForBrickForm brickForm

                        startColumnNumber =
                            middleColumnCellNumber

                        playFieldDictKeys =
                            createPlayFieldDictKeysForBrickForm startRowNumber startColumnNumber brickForm
                    in
                    Ok (BrickModel brickForm direction startRowNumber startColumnNumber playFieldDictKeys True)

                Err error ->
                    Err error

        Err error ->
            Err error


handleKeyPressed : String -> MainModel -> ( MainModel, Cmd Msg )
handleKeyPressed key model =
    if model.playerInput == Possible then
        let
            activeBrickResult =
                isThereCurrentActiveBrick model.gameModel.currentBrickModel
        in
        case activeBrickResult of
            Ok _ ->
                let
                    maybeCommand =
                        case key of
                            "a" ->
                                Just MoveLeft

                            "d" ->
                                Just MoveRight

                            "s" ->
                                Just DropBrickByPlayer

                            "w" ->
                                Just SwitchForm

                            _ ->
                                Nothing
                in
                case maybeCommand of
                    Nothing ->
                        ( model, Cmd.none )

                    Just gameCommand ->
                        let
                            newGameModel =
                                addGameCommandToBackOfGameModelClock gameCommand model.gameModel
                        in
                        ( { model | gameModel = newGameModel, playerInput = Stopped }
                        , Process.sleep activatePlayerInPutWaitTime |> Task.perform (always ActivatePlayerInput)
                        )

            Err _ ->
                ( { model | error = Just "Cant press keys, brick is not active" }
                , Cmd.none
                )

    else
        ( model, Cmd.none )


handleScreenSize : Float -> Float -> MainModel -> ( MainModel, Cmd Msg )
handleScreenSize width height model =
    let
        newSize =
            Size width height
    in
    ( { model | windowSize = newSize }, Cmd.none )
