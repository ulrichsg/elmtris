module Field exposing (..)

import Random

import Block exposing (Block)
import Square exposing (Shape(..))
import Structure exposing (Structure)

type alias Field =
    { width: Int
    , height: Int
    , gridSize: Int
    , sidebarWidth: Int
    }

maxX field = field.width // field.gridSize - 1
maxY field = field.height // field.gridSize - 1

spawnBlock: Random.Seed -> (Block, Random.Seed)
spawnBlock seed =
    let
        shapeGenerator = Random.int 0 6
        rotGenerator = Random.int 0 3
        (shapeRoll, newSeed_) = Random.step shapeGenerator seed
        (rotation, newSeed) = Random.step rotGenerator newSeed_
        shape = case shapeRoll of
            0 -> I
            1 -> O
            2 -> T
            3 -> S
            4 -> T
            5 -> L
            6 -> J
            _ -> T
        block =
            { shape = shape
            , rotation = rotation
            , x = 0
            , y = 0
            }
    in
        (block, newSeed)

dropBlock: Field -> Structure -> Block -> Block
dropBlock field structure block =
    { block
    | x = startingX field block
    , y = startingY field structure block
    }

blockOutOfBounds: Block -> Field -> Bool
blockOutOfBounds block field =
    let
        outOfBounds square =
            square.x < 0 || square.x > maxX field
            || square.y < 0 || square.y > maxY field
    in
    List.any outOfBounds (Block.toSquares block)

startingX field block = (maxX field - Block.width block) // 2 - 1

startingY field structure block =
    let
        x = startingX field block
        startingY_ y =
            if Structure.overlaps { block | x = x, y = y } structure
            then startingY_ (y - 1)
            else y
    in
    startingY_ 0