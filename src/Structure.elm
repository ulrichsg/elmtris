module Structure exposing (..)

import Types exposing (..)
import Block

import Collage exposing (filled, move)
import Debug
import String

fixBlock: Model -> Model
fixBlock model =
    let
        toColoredSquares block x' y' =
            let
                squares = Block.squares block.shape |> Block.rotate block.rotation
                toColoredSquare = \{x, y} -> { x = x + x', y = y + y', color = Block.color block.shape }
            in
                List.map toColoredSquare squares
        newSquares = toColoredSquares model.currentBlock model.x model.y
    in
        { model | structure = List.append newSquares model.structure }

render: Model -> List Collage.Form
render model =
    let
        gridSize = model.field.gridSize
        renderSquare = \square ->
            let
                sx = square.x * gridSize + 10
                sy = square.y * gridSize + 10
            in
                Collage.square gridSize
                    |> filled square.color
                    |> move (toFloat sx, toFloat sy)
    in
        List.map renderSquare model.structure


blockToSquares model =
    let
        transformSquares = List.map (\{x, y} -> { x = x + model.x, y = y + model.y })
    in
        model.currentBlock.shape
            |> Block.squares
            |> Block.rotate model.currentBlock.rotation
            |> transformSquares

tryAll f structure squares =
    let
        tryOne structure square = List.any (f square) structure
    in
        List.any (tryOne structure) squares


touchesFromAbove: Model -> Bool
touchesFromAbove model =
    let
        touchesFromAbove' square1 square2 = square1.x == square2.x && square1.y == square2.y + 1
    in
        tryAll touchesFromAbove' model.structure (blockToSquares model)

touchesFromLeft: Model -> Bool
touchesFromLeft model =
    let
        touchesFromLeft' square1 square2 = square1.y == square2.y && square1.x == square2.x - 1
    in
        tryAll touchesFromLeft' model.structure (blockToSquares model)

touchesFromRight: Model -> Bool
touchesFromRight model =
    let
        touchesFromRight' square1 square2 = square1.y == square2.y && square1.x == square2.x + 1
    in
        tryAll touchesFromRight' model.structure (blockToSquares model)


removeCompletedLines model =
    let
        range a b = if a > b then [] else [a] ++ (range (a+1) b)
        allYCoords = range (bottom model.field) (top model.field)

        countSquares y' = model.structure
            |> List.filter (\{y} -> y == y')
            |> List.length
        widthSquares = model.field.width // model.field.gridSize
        isCompleted y = countSquares y >= widthSquares

        completedLines = List.filter isCompleted allYCoords

        removeLines ys structure = List.filter (\{y} -> not (List.member y ys)) structure

        dropSquareIfAbove y square = if square.y > y then { square | y = square.y - 1 } else square
        dropLinesAbove' y = List.map (dropSquareIfAbove y)
        dropLinesAbove ys structure = List.foldr dropLinesAbove' structure ys
    in
        { model |
            structure = model.structure
                |> removeLines completedLines
                |> dropLinesAbove completedLines,
            lines = model.lines + List.length completedLines
        }

checkGameOver model =
    let
        upperBound = top model.field
        touchesTop' square = square.y > upperBound
        touchesTop model = List.any touchesTop' (blockToSquares model)
        isGameOver model = touchesTop model && touchesFromAbove model
    in
        if (isGameOver model)
        then { model |
            status = GameOver
        }
        else model
