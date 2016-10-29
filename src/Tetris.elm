import Html.App as App
import AnimationFrame
import Keyboard
import Random.Pcg as Random

import Types exposing (..)
import Block
import Structure
import View

type Msg = KeyDown Int | Tick Float

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
    case (model.status, action) of
        (Running, KeyDown key) -> (control key model, Cmd.none)
        (Running, Tick dt) -> (step dt model, Cmd.none)
        _ -> (model, Cmd.none)

step: Float -> Model -> Model
step dt model =
    let
        newModel = { model | t = model.t + dt }
        fall model = { model | y = model.y - 1, lastDrop = model.t }
    in
        if newModel.t >= model.lastDrop + model.dropInterval
        then newModel
            |> checkBlockLanded
            |> fall
        else newModel

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

moveBlock: (Model -> Model) -> Model -> Model
moveBlock op model =
    let
        newModel = op model
    in
        if Structure.overlaps newModel || Structure.blockOutOfBounds newModel
        then model
        else newModel

moveLeft = moveBlock (\model -> { model | x = model.x - 1, lastMove = model.t })
moveRight = moveBlock (\model -> { model | x = model.x + 1, lastMove = model.t })
moveDown = moveBlock (\model -> { model | y = model.y - 1, lastMove = model.t })

rotate model =
    let
        currentBlock = model.currentBlock
        rotatedBlock = { currentBlock | rotation = (currentBlock.rotation + 1) % 4 }
    in
        moveBlock (\model -> { model | currentBlock = rotatedBlock }) model

checkBlockLanded model =
    if model.y <= bottom model.field || Structure.touchesFromAbove model
    then processBlockLanded(model)
    else model

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
        }
    in
        model
            |> Structure.fixBlock
            |> Structure.removeCompletedLines
            |> Structure.checkGameOver
            |> checkNextLevel
            |> dropNextBlock

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
