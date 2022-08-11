module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events as BE
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as JD
import Random
import Set exposing (Set)


monsterWidth : Float
monsterWidth =
    20


bulletWidth : Float
bulletWidth =
    4


bulletSpeed : Float
bulletSpeed =
    5


type alias GameMap =
    Array (Array String)


gameMap : GameMap
gameMap =
    [ "x    xxxxxxxxxxxxxxx"
    , "x b  x             x"
    , "x    x     b       x"
    , "x    x             x"
    , "x    x  xxxxxxxx   x"
    , "x    x             x"
    , "x    x             x"
    , "x        b         x"
    , "x       xxx  b     x"
    , "x  xxx             x"
    , "x    x        x    x"
    , "x    x        x    x"
    , "xxxxxx        x  b x"
    , "x    x        x    x"
    , "x p  x             x"
    , "x    x             x"
    , "x    x             x"
    , "x          xxx     x"
    , "x                  x"
    , "xxxxxxxxxxxxxxxxxxxx"
    ]
        |> Array.fromList
        |> Array.map (String.split "" >> Array.fromList)


type alias Model =
    { lastUpdate : Float
    , keysDown : Set String
    , x : Float
    , y : Float
    , mouseDown : Maybe ( Float, Float )
    , bullets : List Bullet
    , monsters : List Monster
    , monsterBullets : List Bullet
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
      , monsterBullets = []
      }
    , Cmd.none
    )


type Msg
    = Tick Float
    | KeyDown String
    | KeyUp String
    | MouseDown ( Float, Float )
    | MouseUp
    | MonsterBullets (List Bullet)


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

                newMonsterBullets =
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
                                    || isOverlapping bullet { x = model.x, y = model.y }
                            then
                                Nothing

                            else
                                Just { bullet | x = newX, y = newY }
                        )
                        model.monsterBullets

                generateMonsterBullets =
                    Random.list (List.length model.monsters)
                        (Random.map (\x -> x <= 5) (Random.int 1 100))
                        |> Random.map
                            (\bools ->
                                List.filterMap
                                    (\( bool, mon ) ->
                                        if bool then
                                            Just (makeBullet mon model)

                                        else
                                            Nothing
                                    )
                                    (List.map2 (\x y -> ( x, y )) bools model.monsters)
                            )
                        |> Random.generate MonsterBullets
            in
            ( { model
                | y = clamp 0 400 (model.y + dy)
                , x = clamp 0 400 (model.x + dx)
                , bullets = newBullets
                , monsters = newMonsters
                , monsterBullets = newMonsterBullets
              }
            , generateMonsterBullets
            )

        KeyDown key ->
            ( { model | keysDown = Set.insert key model.keysDown }, Cmd.none )

        KeyUp key ->
            ( { model | keysDown = Set.remove key model.keysDown }, Cmd.none )

        MouseDown ( mouseX, mouseY ) ->
            ( { model
                | mouseDown = Just ( mouseX, mouseY )
                , bullets = makeBullet model { x = mouseX, y = mouseY } :: model.bullets
              }
            , Cmd.none
            )

        MouseUp ->
            ( { model | mouseDown = Nothing }, Cmd.none )

        MonsterBullets newMonsterBullets ->
            ( { model | monsterBullets = model.monsterBullets ++ newMonsterBullets }, Cmd.none )


isOverlapping : Bullet -> { a | x : Float, y : Float } -> Bool
isOverlapping bullet monster =
    ((bullet.x + bulletWidth > monster.x) && (bullet.x < monster.x + monsterWidth))
        && ((bullet.y + bulletWidth > monster.y) && (bullet.y < monster.y + monsterWidth))


makeBullet : { a | x : Float, y : Float } -> { b | x : Float, y : Float } -> Bullet
makeBullet a b =
    let
        dx =
            b.x - a.x

        dy =
            b.y - a.y

        angle =
            atan (dy / dx)

        sign =
            if dx < 0 then
                -1

            else
                1
    in
    Bullet a.x a.y (sign * bulletSpeed * cos angle) (sign * bulletSpeed * sin angle)


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
                (model.bullets ++ model.monsterBullets)
            )
        , viewGameMap gameMap
        ]


viewGameMap : GameMap -> Html msg
viewGameMap map =
    Html.div []
        (List.range 0 (Array.length map - 1)
            |> List.concatMap
                (\y ->
                    List.range 0 (Array.length map - 1)
                        |> List.map (\x -> ( x, y ))
                )
            |> List.filterMap
                (\( x, y ) ->
                    case Array.get y gameMap |> Maybe.andThen (Array.get x) of
                        Nothing ->
                            Nothing

                        Just "x" ->
                            Just
                                (Html.div
                                    [ HA.style "position" "absolute"
                                    , HA.style "left" (px (toFloat x * 20))
                                    , HA.style "top" (px (toFloat y * 20))
                                    , HA.style "background-color" "purple"
                                    , HA.style "width" "20px"
                                    , HA.style "height" "20px"
                                    ]
                                    []
                                )

                        Just _ ->
                            Nothing
                )
        )


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
