module BSPMap exposing (BSPMap, LeafData, Position, Size, addPosition, addSize, getLeafs, getMapSize, getPosition, getSize, makeMap, position, size)

import Array as Array exposing (Array)
import Random as Random exposing (Generator, Seed)
import Random.Extra exposing (choice)


type BSPMap
    = Branch BranchData
    | Leaf LeafData


type alias BranchData =
    { position : Position
    , size : Size
    , left : BSPMap
    , right : BSPMap
    }


type alias LeafData =
    { position : Position
    , size : Size
    }


type Size
    = Size ( Int, Int )


type Position
    = Position ( Int, Int )


type SplitDirection
    = Horizontal
    | Vertical
    | NoSplit


makeMap : Position -> Size -> Size -> Int -> Seed -> ( BSPMap, Seed )
makeMap pos mapSize leafSize recursionLevel seed =
    let
        leafData =
            LeafData pos mapSize
    in
    if recursionLevel == 0 then
        ( Leaf leafData, seed )

    else
        case getSplitDirection leafSize leafData seed of
            ( Horizontal, hSeed ) ->
                splitHorizontal leafData leafSize recursionLevel hSeed

            ( Vertical, vSeed ) ->
                splitVertical leafData leafSize recursionLevel vSeed

            ( NoSplit, nSeed ) ->
                ( Leaf leafData, nSeed )


getLeafs : BSPMap -> List LeafData
getLeafs map =
    case map of
        Leaf leafData ->
            [ leafData ]

        Branch branchData ->
            List.append (getLeafs branchData.left) (getLeafs branchData.right)


getSplitDirection : Size -> LeafData -> Seed -> ( SplitDirection, Seed )
getSplitDirection (Size leafSize) leafData seed =
    let
        ( width, height ) =
            getSize leafData.size

        ( leafWidth, leafHeight ) =
            leafSize

        canSplitHorizontal =
            height > 2 * leafHeight + 4

        canSplitVertical =
            width > 2 * leafWidth + 4

        canSplitBothWays =
            canSplitHorizontal && canSplitVertical
    in
    if canSplitBothWays then
        randomSplitDirection seed

    else if canSplitHorizontal then
        ( Horizontal, seed )

    else if canSplitVertical then
        ( Vertical, seed )

    else
        ( NoSplit, seed )


splitHorizontal : LeafData -> Size -> Int -> Seed -> ( BSPMap, Seed )
splitHorizontal leafData leafSize recursionLevel seed =
    let
        ( width, height ) =
            getSize leafData.size

        ( _, leafHeight ) =
            getSize leafSize

        ( leafX, leafY ) =
            getPosition leafData.position

        ( y, seed2 ) =
            randomBetween (leafHeight + 2) (height - leafHeight - 2) seed

        leftPosition =
            leafData.position

        leftMapSize =
            Size ( width, height - y )

        rightPosition =
            Position ( leafX, leafY + height - y )

        rightMapSize =
            Size ( width, y )

        ( leftMap, seed3 ) =
            makeMap leftPosition leftMapSize leafSize (recursionLevel - 1) seed2

        ( rightMap, seed4 ) =
            makeMap rightPosition rightMapSize leafSize (recursionLevel - 1) seed3

        branchData =
            BranchData leafData.position leafData.size leftMap rightMap
    in
    ( Branch branchData, seed4 )


splitVertical : LeafData -> Size -> Int -> Seed -> ( BSPMap, Seed )
splitVertical leafData leafSize recursionLevel seed =
    let
        ( width, height ) =
            getSize leafData.size

        ( leafWidth, _ ) =
            getSize leafSize

        ( leafX, leafY ) =
            getPosition leafData.position

        ( x, seed2 ) =
            randomBetween (leafWidth + 2) (width - leafWidth - 2) seed

        leftPosition =
            leafData.position

        leftMapSize =
            Size ( width - x, height )

        rightPosition =
            Position ( leafX + width - x, leafY )

        rightMapSize =
            Size ( x, height )

        ( leftMap, seed3 ) =
            makeMap leftPosition leftMapSize leafSize (recursionLevel - 1) seed2

        ( rightMap, seed4 ) =
            makeMap rightPosition rightMapSize leafSize (recursionLevel - 1) seed3

        branchData =
            BranchData leafData.position leafData.size leftMap rightMap
    in
    ( Branch branchData, seed4 )


size : ( Int, Int ) -> Size
size s =
    Size s


getSize : Size -> ( Int, Int )
getSize (Size s) =
    s


addSize : ( Int, Int ) -> Size -> Size
addSize ( w, h ) s =
    let
        ( width, height ) =
            getSize s
    in
    size ( w + width, h + height )


position : ( Int, Int ) -> Position
position pos =
    Position pos


getPosition : Position -> ( Int, Int )
getPosition (Position pos) =
    pos


addPosition : ( Int, Int ) -> Position -> Position
addPosition ( x, y ) pos =
    let
        ( x2, y2 ) =
            getPosition pos
    in
    position ( x + x2, y + y2 )


getMapSize : BSPMap -> Size
getMapSize map =
    case map of
        Branch branchData ->
            branchData.size

        Leaf leafData ->
            leafData.size


randomSplitDirection : Seed -> ( SplitDirection, Seed )
randomSplitDirection seed =
    Random.step splitDirectionGenerator seed


splitDirectionGenerator : Generator SplitDirection
splitDirectionGenerator =
    choice Horizontal Vertical


randomBetween : Int -> Int -> Seed -> ( Int, Seed )
randomBetween n m seed =
    Random.step (Random.int n m) seed
