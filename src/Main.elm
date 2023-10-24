module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Functions.Base exposing (addToFrontOfList)
import Functions.Brick exposing (getStartRowNumberForBrickForm)
import Functions.GameClock exposing (tickGameClock)
import Functions.GameCommand exposing (executeGameCommand)
import Functions.GameModel exposing (trySetNewBrickInGameModel)
import Functions.Playfield exposing (createPlayFieldDictKeysForBrickForm)
import Functions.Random exposing (rollRandomBrickModel, tryGetBrickForm, tryGetRandomDirection)
import Messages exposing (Msg(..))
import Models exposing (BrickForm(..), BrickModel, Cell, Direction(..), GameCommand(..), Model, Size, startSize)
import PlayFieldGenerator exposing (initGameModel)
import PlayFieldSizes exposing (middleColumnCellNumber)
import Process
import Random
import Task
import Timers exposing (brickDropTime, gameClockWaitTime)
import View exposing (view)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    ( { windowSize = startSize
      , gameModel = initGameModel
      , error = Nothing
      }
    , Task.perform GotViewport Browser.Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewPort ->
            handleScreenSize viewPort.viewport.width viewPort.viewport.height model

        StartGame ->
            ( model
            , Cmd.batch
                [ Random.generate NewBrick rollRandomBrickModel
                , Task.perform (\_ -> StartClock) (Task.succeed True)
                ]
            )

        StartClock ->
            update Tick model

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
                        oldGameModel =
                            model.gameModel

                        newGameModelResult =
                            executeGameCommand nextCommand { oldGameModel | gameClock = newGameClock }
                    in
                    case newGameModelResult of
                        Ok newGameModel ->
                            ( { model | gameModel = newGameModel }, nextTickCmd )

                        Err error ->
                            ( { model | error = Just error }, Cmd.none )

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
                                fallingBrickCommand =
                                    Process.sleep brickDropTime |> Task.perform (always DropCurrentBrick)
                            in
                            ( { model | gameModel = newGameModel }, fallingBrickCommand )

                        Err error ->
                            ( { model | error = Just error }, Cmd.none )

                Err error ->
                    ( { model | error = Just error }, Cmd.none )

        DropCurrentBrick ->
            let
                newGameClock =
                    addToFrontOfList DropBrick model.gameModel.gameClock

                oldGameModel =
                    model.gameModel

                newGameModel =
                    { oldGameModel | gameClock = newGameClock }
            in
            ( { model | gameModel = newGameModel }
            , Process.sleep brickDropTime |> Task.perform (always DropCurrentBrick)
            )


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
                    Ok (BrickModel brickForm direction startRowNumber startColumnNumber playFieldDictKeys)

                Err error ->
                    Err error

        Err error ->
            Err error


handleScreenSize : Float -> Float -> Model -> ( Model, Cmd Msg )
handleScreenSize width height model =
    let
        newSize =
            Size width height
    in
    ( { model | windowSize = newSize }, Cmd.none )
