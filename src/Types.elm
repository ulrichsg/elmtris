module Types exposing (..)

import Color exposing (Color)
import Random.Pcg as Random

type Status = Ready | Running | GameOver

type alias Field = {
    width: Int,
    height: Int,
    gridSize: Int
}

type Shape = I | O | T | S | Z | L | J

type alias Block = {
    shape: Shape,
    rotation: Int
}

type alias Square = { x: Int, y: Int }

type alias ColoredSquare = { x: Int, y: Int, color: Color }
type alias Structure = List ColoredSquare

type alias Model = {
    status: Status,
    field: Field,
    lines: Int,
    currentBlock: Block,
    nextBlock: Block,
    structure: Structure,
    x: Int,
    y: Int,
    t: Float,
    lastDrop: Float,
    dropInterval: Float,
    lastMove: Float,
    moveInterval: Float,
    randomSeed: Random.Seed
}



leftBorder field = round ((toFloat field.width / toFloat field.gridSize) / -2.0)
rightBorder field = (leftBorder field * -1) - 1
bottom field = round ((toFloat field.height / toFloat field.gridSize) / -2.0)
top field = (bottom field * -1) - 1
