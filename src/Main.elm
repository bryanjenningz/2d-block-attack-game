module Main exposing (main)

import Browser
import Browser.Events as BE
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as JD
import Set exposing (Set)


type alias Model =
    { lastUpdate : Float
    , keysDown : Set String
    , x : Float
    , y : Float
    , mouseDown : Maybe ( Float, Float )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { lastUpdate = 0
      , keysDown = Set.empty
      , x = 0
      , y = 0
      , mouseDown = Nothing
      }
    , Cmd.none
    )


type Msg
    = Tick Float
    | KeyDown String
    | KeyUp String
    | MouseDown ( Float, Float )
    | MouseUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            let
                scalar =
                    if
                        List.all (\keys -> List.any (\key -> Set.member key model.keysDown) keys)
                            [ [ "ArrowRight", "ArrowLeft" ], [ "ArrowUp", "ArrowDown" ] ]
                    then
                        1

                    else
                        sqrt 2

                dx =
                    if Set.member "ArrowRight" model.keysDown then
                        scalar

                    else if Set.member "ArrowLeft" model.keysDown then
                        -scalar

                    else
                        0

                dy =
                    if Set.member "ArrowDown" model.keysDown then
                        scalar

                    else if Set.member "ArrowUp" model.keysDown then
                        -scalar

                    else
                        0
            in
            ( { model | y = model.y + dy, x = model.x + dx }, Cmd.none )

        KeyDown key ->
            ( { model | keysDown = Set.insert key model.keysDown }, Cmd.none )

        KeyUp key ->
            ( { model | keysDown = Set.remove key model.keysDown }, Cmd.none )

        MouseDown mouseXY ->
            ( { model | mouseDown = Just mouseXY }, Cmd.none )

        MouseUp ->
            ( { model | mouseDown = Nothing }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div [ HA.class "container" ]
        [ Html.div
            [ HA.class "player"
            , HA.style "left" (px model.x)
            , HA.style "top" (px model.y)
            ]
            []
        , Html.div [ HA.style "color" "white" ]
            [ Html.text
                (case model.mouseDown of
                    Nothing ->
                        "mouseup"

                    Just ( mouseX, mouseY ) ->
                        "mousedown x = " ++ px mouseX ++ ", y = " ++ px mouseY
                )
            ]
        ]


px : Float -> String
px x =
    String.fromFloat x ++ "px"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BE.onAnimationFrameDelta Tick
        , BE.onKeyDown
            (JD.field "key" JD.string
                |> JD.map KeyDown
            )
        , BE.onKeyUp
            (JD.field "key" JD.string
                |> JD.map KeyUp
            )
        , BE.onMouseDown
            (JD.map2 (\x y -> MouseDown ( x, y ))
                (JD.field "clientX" JD.float)
                (JD.field "clientY" JD.float)
            )
        , BE.onMouseUp (JD.succeed MouseUp)
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
