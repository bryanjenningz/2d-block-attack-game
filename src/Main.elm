module Main exposing (main)

import Browser
import Browser.Events as BE
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as JD
import Set exposing (Set)


monsterWidth : Float
monsterWidth =
    20


bulletWidth : Float
bulletWidth =
    4


type alias Model =
    { lastUpdate : Float
    , keysDown : Set String
    , x : Float
    , y : Float
    , mouseDown : Maybe ( Float, Float )
    , bullets : List Bullet
    , monsters : List Monster
    }


type alias Bullet =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    }


type alias Monster =
    { x : Float
    , y : Float
    , health : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { lastUpdate = 0
      , keysDown = Set.empty
      , x = 200
      , y = 200
      , mouseDown = Nothing
      , bullets = []
      , monsters = [ Monster 20 100 100, Monster 300 350 100 ]
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

                newBullets =
                    List.filterMap
                        (\bullet ->
                            let
                                newX =
                                    bullet.x + bullet.vx

                                newY =
                                    bullet.y + bullet.vy
                            in
                            if
                                ((newX < 0) || (newX > 400))
                                    || ((newY < 0) || (newY > 400))
                                    || List.any (isOverlapping bullet) model.monsters
                            then
                                Nothing

                            else
                                Just { bullet | x = newX, y = newY }
                        )
                        model.bullets

                newMonsters =
                    List.filterMap
                        (\monster ->
                            let
                                bulletsHit =
                                    List.filter
                                        (\bullet -> isOverlapping bullet monster)
                                        model.bullets
                                        |> List.length

                                newHealth =
                                    monster.health - toFloat bulletsHit * 20
                            in
                            if newHealth <= 0 then
                                Nothing

                            else
                                Just { monster | health = newHealth }
                        )
                        model.monsters
            in
            ( { model
                | y = clamp 0 400 (model.y + dy)
                , x = clamp 0 400 (model.x + dx)
                , bullets = newBullets
                , monsters = newMonsters
              }
            , Cmd.none
            )

        KeyDown key ->
            ( { model | keysDown = Set.insert key model.keysDown }, Cmd.none )

        KeyUp key ->
            ( { model | keysDown = Set.remove key model.keysDown }, Cmd.none )

        MouseDown ( mouseX, mouseY ) ->
            let
                dx =
                    mouseX - model.x

                dy =
                    mouseY - model.y

                angle =
                    atan (dy / dx)

                sign =
                    if dx < 0 then
                        -1

                    else
                        1
            in
            ( { model
                | mouseDown = Just ( mouseX, mouseY )
                , bullets =
                    Bullet
                        model.x
                        model.y
                        (sign * 5 * cos angle)
                        (sign * 5 * sin angle)
                        :: model.bullets
              }
            , Cmd.none
            )

        MouseUp ->
            ( { model | mouseDown = Nothing }, Cmd.none )


isOverlapping : Bullet -> Monster -> Bool
isOverlapping bullet monster =
    ((bullet.x + bulletWidth > monster.x) && (bullet.x < monster.x + monsterWidth))
        && ((bullet.y + bulletWidth > monster.y) && (bullet.y < monster.y + monsterWidth))


view : Model -> Html Msg
view model =
    Html.div [ HA.class "container" ]
        [ Html.div
            [ HA.class "player"
            , HA.style "left" (px model.x)
            , HA.style "top" (px model.y)
            ]
            []
        , Html.div
            []
            (List.map
                (\monster ->
                    Html.div
                        [ HA.class "monster"
                        , HA.style "left" (px monster.x)
                        , HA.style "top" (px monster.y)
                        ]
                        []
                )
                model.monsters
            )
        , Html.div
            []
            (List.map
                (\bullet ->
                    Html.div
                        [ HA.class "bullet"
                        , HA.style "left" (px bullet.x)
                        , HA.style "top" (px bullet.y)
                        ]
                        []
                )
                model.bullets
            )
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
