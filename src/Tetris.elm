import Html.App as App
import AnimationFrame
import Keyboard
import Random.Pcg as Random

import Types exposing (..)
import Control exposing (control)
import Block
import Structure
import View
import Ports

type Msg = KeyDown Int | Tick Float

type alias Flags = {
    highScore: Int,
    random1: Int,
    random2: Int
}

startingX = -1
startingY field structure block =
    let blockHeight = Block.squares block.shape
            |> Block.rotate block.rotation
            |> Block.height
        startingY' offset =
            let y = top field - blockHeight + offset
            in
                if Structure.overlapsBlock structure block startingX y
                    then startingY' (offset + 1)
                    else y
    in startingY' -1


init: Flags -> (Model, Cmd Msg)
init flags =
    let
        initialSeed = Random.initialSeed2 flags.random1 flags.random2
        (currentBlock, nextSeed) = Block.random initialSeed
        (nextBlock, actualSeed) = Block.random nextSeed

        field = { width = 320, height = 480, gridSize = 20, sidebarWidth = 160 }
        initialModel = {
            status = Running,
            field = field,
            lines = 0,
            level = 1,
            score = 0,
            highScore = flags.highScore,
            currentBlock = currentBlock,
            nextBlock = nextBlock,
            structure = [],
            x = startingX,
            y = startingY field [] currentBlock,
            t = 0.0,
            lastDrop = 0.0,
            dropInterval = 1000.0,
            lastMove = 0.0,
            moveInterval = 50.0,
            randomSeed = actualSeed
        }
    in
        (initialModel, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update action model =
    case (model.status, action) of
        (Running, KeyDown key) -> (control key model, Cmd.none)
        (Running, Tick dt) -> step dt model
        _ -> (model, Cmd.none)

step: Float -> Model -> (Model, Cmd Msg)
step dt model =
    let
        newModel = { model | t = model.t + dt }
        fall model = { model | y = model.y - 1, lastDrop = model.t }

        nextModel = if newModel.t >= model.lastDrop + model.dropInterval
            then newModel
                |> checkBlockLanded
                |> fall
            else newModel
        command = if newModel.status == GameOver
            then Ports.highScore model.highScore
            else Cmd.none
    in
        (nextModel, command)

checkBlockLanded model =
    if model.y <= bottom model.field || Structure.touchesFromAbove model
    then processBlockLanded(model)
    else model

dropNextBlock model =
    let
        newBlock = model.nextBlock
        (nextBlock, nextSeed) = Block.random model.randomSeed
        newModel = { model |
            currentBlock = newBlock,
            nextBlock = nextBlock,
            x = startingX,
            y = startingY model.field model.structure newBlock,
            randomSeed = nextSeed
        }
        gameOver = False
    in
        if gameOver
            then { newModel | status = GameOver, highScore = max model.score model.highScore }
            else newModel

processBlockLanded model =
    let
        newModel = model
            |> Structure.fixBlock
            |> Structure.removeCompletedLines
            |> checkNextLevel
            |> Structure.checkGameOver
    in
        if newModel.status == Running
            then checkBlockLanded (dropNextBlock newModel)
            else newModel

checkNextLevel model =
    if model.lines // 10 >= model.level
    then { model | level = model.level + 1, dropInterval = model.dropInterval * 0.9 }
    else model

subscriptions: Model -> Sub Msg
subscriptions model = Sub.batch [
    Keyboard.downs KeyDown,
    AnimationFrame.diffs Tick
    ]

main: Program Flags
main = App.programWithFlags {
    init = init,
    view = View.render,
    update = update,
    subscriptions = subscriptions
    }
