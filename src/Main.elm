module Main exposing (main)

import Array
import Block
import Browser
import Browser.Events
import Config
import Control exposing (control)
import Field
import Json.Decode as Decode
import Keyboard
import Ports
import Random
import Structure
import Types exposing (..)
import View

type Msg
    = KeyDown Keyboard.Key
    | Tick Float

type alias Flags =
    { highScore : Int
    , randomSeed : Int
    }


init : Flags -> (Model, Cmd Msg)
init flags =
    let
        field =
            { width = Config.fieldWidth
            , height = Config.fieldHeight
            , gridSize = Config.gridSize
            , sidebarWidth = Config.sidebarWidth
            }

        initialSeed = Random.initialSeed flags.randomSeed
        (currentBlock, nextSeed) = Field.spawnBlock initialSeed
        (nextBlock, actualSeed) = Field.spawnBlock nextSeed

        initialModel =
            { status = Running
            , field = field
            , lines = 0
            , level = 1
            , score = 0
            , highScore = flags.highScore
            , currentBlock = currentBlock |> Field.dropBlock field []
            , nextBlock = nextBlock
            , structure = []
            , t = 0.0
            , lastDrop = 0.0
            , dropInterval = Config.initialSpeed
            , lastMove = 0.0
            , moveInterval = Config.sensitivity
            , randomSeed = actualSeed
            }
    in
    (initialModel, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case (model.status, action) of
        (Running, KeyDown key) -> (control key model, Cmd.none)
        (Running, Tick dt) -> step dt model
        _ -> (model, Cmd.none)

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
    if Block.bottomY block >= Field.maxY model.field || Structure.touchesFromAbove block model.structure
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

        widthSquares =
            model.field.width // model.field.gridSize
    in
    countSquares lineNum >= widthSquares


removeCompletedLines model =
    let
        allYCoords = List.range 0 (Field.maxY model.field)
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (Decode.map KeyDown Keyboard.decode)
        , Browser.Events.onAnimationFrameDelta Tick
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = View.render
        , update = update
        , subscriptions = subscriptions
        }
