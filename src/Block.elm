module Block exposing (..)

import Collage exposing (Form, filled, move)
import Color exposing (Color)

import Random.Pcg as Random

import Types exposing (..)

render: Model -> List Form
render model =
    let
        gridSize = model.field.gridSize
        block = model.currentBlock

        renderSquare = \square ->
            let
                sx = (model.x + square.x) * gridSize + 10 - (model.field.sidebarWidth // 2)
                sy = (model.y + square.y) * gridSize + 10
            in
                Collage.square gridSize
                    |> filled (color block.shape)
                    |> move (toFloat sx, toFloat sy)
    in
        block.shape
            |> squares
            |> rotate block.rotation
            |> List.map renderSquare


squares: Shape -> List Square
squares shape =
    let
        p = \x y -> { x = x, y = y }
    in
        case shape of
            I -> [p 0 0, p 1 0, p 2 0, p 3 0]
            O -> [p 0 0, p 1 0, p 0 1, p 1 1]
            T -> [p 0 1, p 1 0, p 1 1, p 1 2]
            S -> [p 0 0, p 0 1, p 1 1, p 1 2]
            Z -> [p 0 1, p 0 2, p 1 0, p 1 1]
            L -> [p 0 0, p 1 0, p 0 1, p 0 2]
            J -> [p 0 0, p 1 0, p 1 1, p 1 2]


color: Shape -> Color
color shape =
    case shape of
        I -> Color.lightBlue
        O -> Color.yellow
        T -> Color.purple
        S -> Color.lightGreen
        Z -> Color.red
        L -> Color.orange
        J -> Color.blue


width = List.foldl (\{x} a -> max a x) 0
height = List.foldl (\{y} a -> max a y) 0

rotate: Int -> List Square -> List Square
rotate times squares =
    let
        dmirror = List.map (\square -> { square | x = square.y, y = square.x })

        hmirror' width square = { square | x = width - square.x }
        hmirror squares = List.map (hmirror' (width squares)) squares

        vmirror' height square = { square | y = height - square.y }
        vmirror squares = List.map (vmirror' (height squares)) squares
    in
        case times of
            0 -> squares
            1 -> hmirror (dmirror squares)
            2 -> hmirror (vmirror squares)
            3 -> vmirror (dmirror squares)
            _ -> squares


random: Random.Seed -> (Block, Random.Seed)
random seed =
    let
        shapeGenerator = Random.int 0 6
        rotGenerator = Random.int 0 3
        (shapeRoll, newSeed') = Random.step shapeGenerator seed
        (rotation, newSeed) = Random.step rotGenerator newSeed'
        shape = case shapeRoll of
            0 -> I
            1 -> O
            2 -> T
            3 -> S
            4 -> T
            5 -> L
            6 -> J
            _ -> T
    in
        ({ shape = shape, rotation = rotation }, newSeed)
