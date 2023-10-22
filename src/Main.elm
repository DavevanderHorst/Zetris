module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Functions.Playfield exposing (setNewBrickInPlayField)
import Functions.Random exposing (rollRandomBrickModel, tryGetBrickForm, tryGetRandomDirection)
import Messages exposing (Msg(..))
import Models exposing (BrickForm(..), BrickModel, Cell, Direction(..), Model, Size, startSize)
import PlayFieldGenerator exposing (createPlayField)
import Random
import Task
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
      , playField = createPlayField
      , tempPlayField = Nothing
      , currentBrickModel = Nothing
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
            ( model, Random.generate NewBrick rollRandomBrickModel )

        NewBrick ( randomBrick, randomDirection ) ->
            case tryMakeBrickModel randomBrick randomDirection of
                Ok newBrickModel ->
                    let
                        newTempPlayField =
                            setNewBrickInPlayField model.playField newBrickModel
                    in
                    ( { model | currentBrickModel = Just newBrickModel, tempPlayField = Just newTempPlayField }, Cmd.none )

                Err error ->
                    ( { model | error = Just error }, Cmd.none )


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
                    Ok (BrickModel brickForm direction)

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
