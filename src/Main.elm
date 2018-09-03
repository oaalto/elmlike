module Main exposing (Model, Msg(..), init, main, update, view)

import Array as Array exposing (Array)
import BSPMap exposing (..)
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Keyboard exposing (Key(..), RawKey, whitespaceKey)
import Random exposing (Generator, Seed, initialSeed)
import Task
import Time exposing (..)



---- MODEL ----


type alias Model =
    { map : TileMap
    }


type Tile
    = Wall
    | Floor


type alias TileRow =
    Array Tile


type alias TileMap =
    Array TileRow


tileToStr : Tile -> String
tileToStr tile =
    case tile of
        Wall ->
            "#"

        Floor ->
            "."


init : ( Model, Cmd Msg )
init =
    let
        ( map, seed ) =
            createMap (initialSeed 10)
    in
    ( Model map, Cmd.none )


createMap : Seed -> ( TileMap, Seed )
createMap seed =
    let
        ( bsp, seed2 ) =
            makeMap (position ( 0, 0 )) (size ( 80, 25 )) (size ( 3, 3 )) 4 seed

        ( map, seed3 ) =
            digDungeonLevel bsp seed2
    in
    ( map, seed3 )



---- UPDATE ----


type Msg
    = KeyUp RawKey
    | GetTime Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyUp rawKey ->
            let
                key =
                    whitespaceKey rawKey
            in
            case key of
                Just k ->
                    if k == Spacebar then
                        ( model, Task.perform GetTime Time.now )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        GetTime time ->
            let
                millis =
                    posixToMillis time

                ( map, seed ) =
                    createMap (initialSeed millis)
            in
            ( { model | map = map }, Cmd.none )


digDungeonLevel : BSPMap -> Seed -> ( TileMap, Seed )
digDungeonLevel map seed =
    let
        ( width, height ) =
            getMapSize map
                |> getSize

        initialMap =
            Array.repeat height (Array.repeat width Wall)

        leafs =
            getLeafs map

        ( mapWithRooms, seed2 ) =
            digRooms leafs seed initialMap
    in
    ( mapWithRooms, seed2 )


digRooms : List LeafData -> Seed -> TileMap -> ( TileMap, Seed )
digRooms leafs seed map =
    List.foldl digRoom ( map, seed ) leafs


digRoom : LeafData -> ( TileMap, Seed ) -> ( TileMap, Seed )
digRoom leaf ( map, seed ) =
    let
        minRoomSize =
            size ( 2, 2 )

        maxRoomSize =
            addSize ( -2, -2 ) leaf.size

        ( room, seed2 ) =
            Debug.log "room size" (randomRoom minRoomSize maxRoomSize seed)

        ( _, height ) =
            getSize room

        ( randPos, seed3 ) =
            Debug.log "room pos" (randomPosition leaf.position leaf.size room seed2)

        ( _, y ) =
            getPosition randPos

        rangeY =
            Debug.log "rangeY" (List.range y (y + height - 1))
    in
    List.foldl (digRow room randPos) ( map, seed3 ) rangeY


digRow : Size -> Position -> Int -> ( TileMap, Seed ) -> ( TileMap, Seed )
digRow roomSize roomPosition y ( map, seed ) =
    let
        ( x, _ ) =
            getPosition roomPosition

        ( width, _ ) =
            getSize roomSize

        rangeX =
            Debug.log "rangeX" (List.range x (x + width - 1))
    in
    List.foldl (digTile y) ( map, seed ) rangeX


digTile : Int -> Int -> ( TileMap, Seed ) -> ( TileMap, Seed )
digTile y x ( map, seed ) =
    let
        row =
            Array.get y map
    in
    case row of
        Just r ->
            let
                newRow =
                    Array.set x Floor r
            in
            ( Array.set y newRow map, seed )

        Nothing ->
            ( map, seed )


randomRoom : Size -> Size -> Seed -> ( Size, Seed )
randomRoom minSize maxSize seed =
    let
        ( minWidth, minHeight ) =
            getSize minSize

        ( maxWidth, maxHeight ) =
            getSize maxSize

        ( width, seed2 ) =
            randomBetween minWidth maxWidth seed

        ( height, seed3 ) =
            randomBetween minHeight maxHeight seed2
    in
    ( size ( width, height ), seed3 )


randomPosition : Position -> Size -> Size -> Seed -> ( Position, Seed )
randomPosition leafPos leafSize roomSize seed =
    let
        ( leafX, leafY ) =
            getPosition leafPos

        ( leafWidth, leafHeight ) =
            getSize leafSize

        ( roomWidth, roomHeight ) =
            getSize roomSize

        startPos =
            addPosition ( 1, 1 ) leafPos

        ( startX, startY ) =
            getPosition startPos

        endPos =
            position ( leafX + leafWidth - roomWidth - 1, leafY + leafHeight - roomHeight - 1 )

        ( endX, endY ) =
            getPosition endPos

        ( x, seed2 ) =
            randomBetween startX endX seed

        ( y, seed3 ) =
            randomBetween startY endY seed2
    in
    ( position ( x, y ), seed3 )


randomBetween : Int -> Int -> Seed -> ( Int, Seed )
randomBetween n m seed =
    Random.step (Random.int n m) seed



---- VIEW ----


view : Model -> Html Msg
view model =
    Element.layout
        []
        (Element.column
            [ centerX ]
            (Array.toList (Array.map viewTileRow model.map))
        )


viewLeaf : LeafData -> Element Msg
viewLeaf leafData =
    Element.text (posToStr leafData.position ++ " " ++ sizeToStr leafData.size)


posToStr : Position -> String
posToStr pos =
    let
        ( x, y ) =
            getPosition pos
    in
    "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"


sizeToStr : Size -> String
sizeToStr s =
    let
        ( width, height ) =
            getSize s
    in
    "(" ++ String.fromInt width ++ ", " ++ String.fromInt height ++ ")"


viewTileRow : TileRow -> Element Msg
viewTileRow row =
    Element.row
        [ Background.color (rgba 0 0 0 1)
        , Font.color (rgba 1 1 1 1)
        , Font.size 32
        , Font.family
            [ Font.monospace
            ]
        ]
        (Array.toList
            (Array.map viewTile row)
        )


viewTile : Tile -> Element Msg
viewTile tile =
    Element.el [] (Element.text (tileToStr tile))



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.ups KeyUp
