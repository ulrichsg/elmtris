module Structure exposing (..)

import Types exposing (..)
import Block

import Collage exposing (filled, move)
import Debug
import String
import Array
import Maybe

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
                sx = square.x * gridSize + 10 - (model.field.sidebarWidth // 2)
                sy = square.y * gridSize + 10
            in
                Collage.square gridSize
                    |> filled square.color
                    |> move (toFloat sx, toFloat sy)
    in
        List.map renderSquare model.structure


currentBlockToSquares model = blockToSquares model.currentBlock model.x model.y

blockToSquares block bx by =
    let
        transformSquares = List.map (\{x, y} -> { x = x + bx, y = y + by })
    in
        block.shape
            |> Block.squares
            |> Block.rotate block.rotation
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
        tryAll touchesFromAbove' model.structure (currentBlockToSquares model)

overlaps: Model -> Bool
overlaps model = overlapsBlock model.structure model.currentBlock model.x model.y

overlapsBlock: Structure -> Block -> Int -> Int -> Bool
overlapsBlock structure block x y =
    let
        equals square1 square2 = square1.x == square2.x && square1.y == square2.y
    in
        tryAll equals structure (blockToSquares block x y)

blockOutOfBounds: Model -> Bool
blockOutOfBounds model =
    let
        field = model.field
        outOfBounds square =
            square.x < leftBorder field || square.x > rightBorder field
            || square.y < bottom field || square.y > top field
    in
        currentBlockToSquares model |> List.any outOfBounds

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
        numCompletedLines = List.length completedLines

        removeLines ys structure = List.filter (\{y} -> not (List.member y ys)) structure

        dropSquareIfAbove y square = if square.y > y then { square | y = square.y - 1 } else square
        dropLinesAbove' y = List.map (dropSquareIfAbove y)
        dropLinesAbove ys structure = List.foldr dropLinesAbove' structure ys
    in
        { model |
            structure = model.structure
                |> removeLines completedLines
                |> dropLinesAbove completedLines,

            lines = model.lines + numCompletedLines,
            score = model.score + points numCompletedLines model.level
        }

points completedLines level =
    let
        basePoints = (Array.fromList [0, 40, 100, 300, 1200]) |> Array.get completedLines |> Maybe.withDefault 0
    in
        basePoints * level

checkGameOver model =
    let
        upperBound = top model.field
        touchesTop' square = square.y > upperBound
        touchesTop model = List.any touchesTop' (currentBlockToSquares model)
        isGameOver model = touchesTop model && touchesFromAbove model
    in
        if (isGameOver model)
        then { model |
            status = GameOver,
            highScore = max model.score model.highScore
        }
        else model
