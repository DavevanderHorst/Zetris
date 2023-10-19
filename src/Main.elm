module Main exposing (main)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events exposing (onResize)
import Html exposing (Html, div)
import Html.Attributes as Attr
import Task


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions baseModel =
    Sub.none


view : Model -> Html Msg
view model =
    div
        [ attrFloat (Attr.style "width") model.windowSize.width
        , attrFloat (Attr.style "height") model.windowSize.height
        ]
        [ div
            [ Attr.style "width" "99%"
            , attrFloat (Attr.style "height") model.windowSize.height
            , Attr.style "padding" "0.5%"
            , Attr.style "display" "flex"
            ]
            []
        ]


attrFloat : (String -> Html.Attribute msg) -> Float -> Html.Attribute msg
attrFloat attr value =
    attr (String.fromFloat value ++ "px")


init : () -> ( Model, Cmd Msg )
init _ =
    ( { windowSize = startSize
      }
    , Task.perform GotViewport Browser.Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotViewport viewPort ->
            handleScreenSize viewPort.viewport.width viewPort.viewport.height model


handleScreenSize : Float -> Float -> Model -> ( Model, Cmd Msg )
handleScreenSize width height model =
    let
        newSize =
            Size width height
    in
    ( { model | windowSize = newSize }, Cmd.none )


startSize : Size
startSize =
    Size 0 0


type Msg
    = GotViewport Viewport


type alias Model =
    -- Main model
    { windowSize : Size
    }


type alias Size =
    { width : Float
    , height : Float
    }
