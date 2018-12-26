module Logic exposing (step)

import Array

import Block
import Field
import Model exposing (..)
import Ports
import Structure

step : Float -> Model -> (Model, Cmd Msg)
step dt model =
    let
        newModel = { model | t = model.t + dt }

        fall aModel =
            let
                block = aModel.currentBlock
                newBlock = { block | y = block.y + 1 }
            in
            { aModel | currentBlock = newBlock, lastDrop = aModel.t }

        nextModel =
            if newModel.t >= model.lastDrop + model.dropInterval
            then
                newModel
                    |> checkBlockLanded
                    |> fall
            else
                newModel

        command =
            if nextModel.status == GameOver
            then Ports.highScore nextModel.highScore
            else Cmd.none
    in
    (nextModel, command)

checkBlockLanded model =
    let
        block = model.currentBlock
    in
    if Block.bottomY block >= model.field.height - 1 || Structure.touchesFromAbove block model.structure
    then processBlockLanded model
    else model

dropNextBlock model =
    let
        newBlock = model.nextBlock
        (nextBlock, nextSeed) = Field.spawnBlock model.randomSeed
    in
    { model
        | currentBlock = newBlock |> Field.dropBlock model.field model.structure
        , nextBlock = nextBlock
        , randomSeed = nextSeed
    }

checkGameOver model =
    let
        block = model.currentBlock
        touchesTop_ square = square.y < 0
        touchesTop m = List.any touchesTop_ (Block.toSquares block)
        isGameOver m = touchesTop m && Structure.touchesFromAbove block model.structure
    in
    if isGameOver model
    then
        { model
            | status = GameOver
            , highScore = max model.score model.highScore
        }
    else
        model

processBlockLanded model =
    let
        newModel =
            { model | structure = Structure.fixBlock model.currentBlock model.structure }
                |> removeCompletedLines
                |> checkNextLevel
                |> checkGameOver
    in
    if newModel.status == Running
    then checkBlockLanded (dropNextBlock newModel)
    else newModel

isLineCompleted model lineNum =
    let
        countSquares l =
            model.structure
                |> List.filter (\{ y } -> y == l)
                |> List.length
    in
    countSquares lineNum >= model.field.width

removeCompletedLines model =
    let
        allYCoords = List.range 0 (model.field.height - 1)
        completedLines = List.filter (isLineCompleted model) allYCoords
        numCompletedLines = List.length completedLines
        removeLines ys structure = List.filter (\{ y } -> not (List.member y ys)) structure

        dropSquareIfAbove y square =
            if square.y < y
            then { square | y = square.y + 1 }
            else square

        dropLinesAbove_ y = List.map (dropSquareIfAbove y)
        dropLinesAbove ys structure = List.foldl dropLinesAbove_ structure ys
    in
    { model
        | structure =
            model.structure
                |> removeLines completedLines
                |> dropLinesAbove completedLines
        , lines = model.lines + numCompletedLines
        , score = model.score + points numCompletedLines model.level
    }

points completedLines level =
    let
        basePoints =
            Array.fromList [0, 40, 100, 300, 1200]
                |> Array.get completedLines
                |> Maybe.withDefault 0
    in
    basePoints * level

checkNextLevel model =
    if model.lines // 10 >= model.level
    then
        { model
        | level = model.level + 1
        , dropInterval = model.dropInterval * 0.9
        }
    else
        model
