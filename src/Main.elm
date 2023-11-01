module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events
import Functions.Brick exposing (createPlayFieldDictKeysForBrickForm, getStartRowNumberForBrickForm, isBrickActive, isThereCurrentActiveBrick)
import Functions.GameClock exposing (tickGameClock)
import Functions.GameCommand exposing (executeGameCommand)
import Functions.GameModel exposing (addGameCommandToBackOfGameModelClock, addGameCommandToFrontOfGameModelClock, emptyGameClock, trySetNewBrickInGameModel)
import Functions.MainModel exposing (setGameClockInMainModel)
import Functions.Random exposing (rollRandomBrickModel, tryGetBrickForm, tryGetRandomDirection)
import Json.Decode as Decode
import Messages exposing (Msg(..))
import Models exposing (BrickModel, Cell, GameCommand(..), MainModel, PlayerInput(..), Size, startSize)
import PlayFieldGenerator exposing (initGameModel)
import PlayFieldSizes exposing (middleColumnCellNumber)
import Process
import Random
import Task
import Timers exposing (activatePlayerInPutWaitTime, brickDropTime, gameClockWaitTime)
import View exposing (view)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : MainModel -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyDown keyDecoder


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

        KeyPressed key ->
            handleKeyPressed key model

        StartGame ->
            ( model
            , Cmd.batch
                [ Random.generate NewBrick rollRandomBrickModel
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

                nextTickCmd =
                    Process.sleep gameClockWaitTime |> Task.perform (always Tick)
            in
            case nextMaybeGameCommand of
                Nothing ->
                    ( model, nextTickCmd )

                Just nextCommand ->
                    let
                        newModel =
                            setGameClockInMainModel newGameClock model
                    in
                    executeGameCommand nextCommand newModel nextTickCmd

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

                                fallingBrickCommand =
                                    Process.sleep brickDropTime |> Task.perform (always DropCurrentBrick)
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
                ( { model | gameModel = newGameModel }
                , Process.sleep brickDropTime |> Task.perform (always DropCurrentBrick)
                )

            else
                ( model, Random.generate NewBrick rollRandomBrickModel )


tryMakeBrickModel : Int -> Int -> Result String BrickModel
tryMakeBrickModel randomBrickForm randomDirection =
    let
        brickFormResult =
            tryGetBrickForm randomBrickForm

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
                                Just DropBrick

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
