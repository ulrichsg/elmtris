import Html.App as App
import AnimationFrame
import Keyboard
import Random.Pcg as Random

import Types exposing (..)
import Block
import Structure
import View

type Msg = KeyDown Int | Tick Float | BlockLanded

startingY field block =
    let blockHeight = Block.squares block.shape
        |> Block.rotate block.rotation
        |> Block.height
    in
        top field - blockHeight - 1

init: Model
init =
    let
        initialSeed = Random.initialSeed2 227852860 149709020
        (currentBlock, nextSeed) = Block.random initialSeed
        (nextBlock, actualSeed) = Block.random nextSeed

        field = { width = 320, height = 480, gridSize = 20, sidebarWidth = 160 }
    in
        {
            status = Running,
            field = field,
            lines = 0,
            level = 1,
            score = 0,
            currentBlock = currentBlock,
            nextBlock = nextBlock,
            structure = [],
            x = -1,
            y = startingY field currentBlock,
            t = 0.0,
            lastDrop = 0.0,
            dropInterval = 1000.0,
            lastMove = 0.0,
            moveInterval = 50.0,
            randomSeed = actualSeed,
            debug = ["Game started."]
        }

update: Msg -> Model -> (Model, Cmd Msg)
update action model =
    if model.status == Running then
        case action of
            KeyDown key -> (control key model, Cmd.none)
            Tick dt -> step dt model
            BlockLanded -> (processBlockLanded model, Cmd.none)
    else
        (model, Cmd.none)


processBlockLanded model =
    let
        (nextBlock, nextSeed) = Block.random model.randomSeed
        y = startingY model.field model.nextBlock
        dropNextBlock model = { model |
            currentBlock = model.nextBlock,
            nextBlock = nextBlock,
            x = -1,
            y = y,
            randomSeed = nextSeed
            -- debug = model.debug ++ ["Dropping new " ++ (toString model.nextBlock.shape) ++ " block at y = " ++ (toString y)]
        }
    in
        model
            |> Structure.removeCompletedLines -- todo make a separate command for this
            |> Structure.checkGameOver
            |> checkNextLevel
            |> dropNextBlock

step: Float -> Model -> (Model, Cmd Msg)
step dt model =
    { model | t = model.t + dt }
        |> fall
        |> checkBlockLanded

control key model =
    if model.t >= model.lastMove + model.moveInterval
    then
        case key of
            37 -> moveLeft model
            38 -> rotate model
            39 -> moveRight model
            40 -> moveDown model
            _ -> model
        else model

blockWidth block = block.shape |> Block.squares |> Block.rotate block.rotation |> Block.width
blockHeight block = block.shape |> Block.squares |> Block.rotate block.rotation |> Block.height

moveLeft model =
    if model.x <= leftBorder model.field || Structure.touchesFromRight model
    then model
    else { model | x = model.x - 1, lastMove = model.t }

moveRight model =
    if model.x + blockWidth model.currentBlock >= rightBorder model.field || Structure.touchesFromLeft model
    then model
    else { model | x = model.x + 1, lastMove = model.t }

moveDown model =
    if model.y <= bottom model.field || Structure.touchesFromAbove model
    then model
    else { model | y = model.y - 1, lastMove = model.t }

rotate model =
    let
        currentBlock = model.currentBlock
        rotatedBlock = { currentBlock | rotation = (currentBlock.rotation + 1) % 4 }
        newModel = { model | currentBlock = rotatedBlock }
    in
        if Structure.overlaps newModel || Structure.blockOutOfBounds newModel
        then model
        else newModel

fall model =
    let
        isDrop = model.t >= model.lastDrop + model.dropInterval
    in
        { model |
            y = if isDrop then model.y - 1 else model.y,
            lastDrop = if isDrop then model.t else model.lastDrop
        }

checkBlockLanded model =
    let
        isLanded = (model.y <= bottom model.field) || Structure.touchesFromAbove model
    in
        if isLanded
        then model |> Structure.fixBlock |> update BlockLanded
        else (model, Cmd.none)

checkNextLevel model =
    if model.lines // 10 >= model.level
    then { model | level = model.level + 1, dropInterval = model.dropInterval * 0.9 }
    else model


subscriptions: Model -> Sub Msg
subscriptions model = Sub.batch [
    Keyboard.downs KeyDown,
    AnimationFrame.diffs Tick
    ]

main: Program Never
main = App.program {
    init = (init, Cmd.none),
    view = View.render,
    update = update,
    subscriptions = subscriptions
    }
