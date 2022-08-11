module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events as BE
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as JD
import Random
import Set exposing (Set)


tileWidth : Float
tileWidth =
    20


bulletWidth : Float
bulletWidth =
    4


bulletSpeed : Float
bulletSpeed =
    5


bulletDamage : Float
bulletDamage =
    20


type alias GameMap =
    Array (Array String)


gameMap : GameMap
gameMap =
    -- x = wall
    -- m = monster spawn location
    -- p = player spawn location
    [ "x    xxxxxxxxxxxxxxx"
    , "x m  x             x"
    , "x    x     m       x"
    , "x    x             x"
    , "x    x  xxxxxxxx   x"
    , "x    x             x"
    , "x    x             x"
    , "x        m         x"
    , "x       xxx  m     x"
    , "x  xxx             x"
    , "x    x        x    x"
    , "x    x        x    x"
    , "xxxxxx        x  m x"
    , "x    x        x    x"
    , "x p  x             x"
    , "x    x             x"
    , "x         xxx      x"
    , "x                  x"
    , "x                  x"
    , "xxxxxxxxxxxxxxxxxxxx"
    ]
        |> Array.fromList
        |> Array.map (String.split "" >> Array.fromList)


type alias Model =
    { keysDown : Set String
    , x : Float
    , y : Float
    , bullets : List Bullet
    , monsters : List Monster
    , monsterBullets : List Bullet
    }


type alias Bullet =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , w : Float
    }


type alias Monster =
    { x : Float
    , y : Float
    , health : Float
    , w : Float
    , vx : Float
    , vy : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        player =
            gameMapPieces "p"
                |> List.head
                |> Maybe.withDefault { x = 200, y = 200, w = tileWidth }

        monsters =
            gameMapPieces "m"
                |> List.map
                    (\{ x, y } -> { x = x, y = y, health = 100, w = tileWidth, vx = 0, vy = 0 })
    in
    ( { keysDown = Set.empty
      , x = player.x
      , y = player.y
      , bullets = []
      , monsters = monsters
      , monsterBullets = []
      }
    , Cmd.none
    )


type Msg
    = Tick
    | KeyDown String
    | KeyUp String
    | MouseDown ( Float, Float )
    | MonsterBullets (List Bullet)
    | MonsterVelocities (List (Maybe ( Float, Float )))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
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

                newPlayerX =
                    if
                        List.any
                            (\wall -> isOverlapping wall { x = model.x + dx, y = model.y, w = tileWidth })
                            gameMapWalls
                    then
                        model.x

                    else
                        model.x + dx

                newPlayerY =
                    if
                        List.any
                            (\wall -> isOverlapping wall { x = model.x, y = model.y + dy, w = tileWidth })
                            gameMapWalls
                    then
                        model.y

                    else
                        model.y + dy

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
                                    || List.any (isOverlapping bullet) gameMapWalls
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
                                    monster.health - toFloat bulletsHit * bulletDamage

                                { x, y } =
                                    let
                                        updatedMonster =
                                            { monster
                                                | x = monster.x + monster.vx
                                                , y = monster.y + monster.vy
                                            }
                                    in
                                    if List.any (isOverlapping updatedMonster) gameMapWalls then
                                        monster

                                    else
                                        updatedMonster
                            in
                            if newHealth <= 0 then
                                Nothing

                            else
                                Just { monster | health = newHealth, x = x, y = y }
                        )
                        model.monsters

                generateNewMonsterVelocities =
                    Random.list (List.length newMonsters)
                        (Random.weighted ( 90, Nothing )
                            [ ( 1, Just ( 0, 0 ) )
                            , ( 1, Just ( 0, 1 ) )
                            , ( 1, Just ( 0, -1 ) )
                            , ( 1, Just ( 1, 0 ) )
                            , ( 1, Just ( -1, 0 ) )
                            ]
                        )
                        |> Random.generate MonsterVelocities

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
                                    || isOverlapping bullet { x = model.x, y = model.y, w = tileWidth }
                                    || List.any (isOverlapping bullet) gameMapWalls
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
                | x = newPlayerX
                , y = newPlayerY
                , bullets = newBullets
                , monsters = newMonsters
                , monsterBullets = newMonsterBullets
              }
            , Cmd.batch [ generateMonsterBullets, generateNewMonsterVelocities ]
            )

        KeyDown key ->
            ( { model | keysDown = Set.insert key model.keysDown }, Cmd.none )

        KeyUp key ->
            ( { model | keysDown = Set.remove key model.keysDown }, Cmd.none )

        MouseDown ( mouseX, mouseY ) ->
            ( { model | bullets = makeBullet model { x = mouseX, y = mouseY } :: model.bullets }
            , Cmd.none
            )

        MonsterBullets newMonsterBullets ->
            ( { model | monsterBullets = model.monsterBullets ++ newMonsterBullets }, Cmd.none )

        MonsterVelocities newVelocities ->
            ( { model
                | monsters =
                    List.map2
                        (\maybeVelocity monster ->
                            case maybeVelocity of
                                Nothing ->
                                    monster

                                Just ( vx, vy ) ->
                                    { monster | vx = vx, vy = vy }
                        )
                        newVelocities
                        model.monsters
              }
            , Cmd.none
            )


isOverlapping : { a | x : Float, y : Float, w : Float } -> { b | x : Float, y : Float, w : Float } -> Bool
isOverlapping a b =
    ((a.x + a.w > b.x) && (a.x < b.x + b.w))
        && ((a.y + a.w > b.y) && (a.y < b.y + b.w))


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
    Bullet a.x a.y (sign * bulletSpeed * cos angle) (sign * bulletSpeed * sin angle) bulletWidth


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
        , viewGameMapWalls
        ]


gameMapPieces : String -> List { x : Float, y : Float, w : Float }
gameMapPieces piece =
    List.range 0 (Array.length gameMap - 1)
        |> List.concatMap
            (\y ->
                List.range 0 (Array.length gameMap - 1)
                    |> List.map (\x -> ( x, y ))
            )
        |> List.filterMap
            (\( x, y ) ->
                case Array.get y gameMap |> Maybe.andThen (Array.get x) of
                    Just tile ->
                        if tile == piece then
                            Just { x = toFloat x * tileWidth, y = toFloat y * tileWidth, w = tileWidth }

                        else
                            Nothing

                    Nothing ->
                        Nothing
            )


gameMapWalls : List { x : Float, y : Float, w : Float }
gameMapWalls =
    gameMapPieces "x"


viewGameMapWalls : Html msg
viewGameMapWalls =
    Html.div []
        (List.map
            (\{ x, y } ->
                Html.div
                    [ HA.style "position" "absolute"
                    , HA.style "left" (px x)
                    , HA.style "top" (px y)
                    , HA.style "background-color" "purple"
                    , HA.style "width" (px tileWidth)
                    , HA.style "height" (px tileWidth)
                    ]
                    []
            )
            gameMapWalls
        )


px : Float -> String
px x =
    String.fromFloat x ++ "px"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ BE.onAnimationFrame (\_ -> Tick)
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
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
