import Html exposing (..)
import Html.App as App
import AnimationFrame
import Keyboard
import Random.Pcg as Random

import Collage exposing (..)
import Element exposing (..)
import Color exposing (black)
import Text

import Types exposing (..)
import Block
import Structure

type Msg = KeyDown Int | Tick Float

startingY field block =
    let blockHeight = Block.squares block.shape
        |> Block.rotate block.rotation
        |> Block.height
    in
        top field -  blockHeight

init: Model
init =
    let
        initialSeed = Random.initialSeed2 227852860 149709020
        (currentBlock, nextSeed) = Block.random initialSeed
        (nextBlock, actualSeed) = Block.random nextSeed

        field = { width = 400, height = 600, gridSize = 20 }
    in
        {
            status = Running,
            field = field,
            lines = 0,
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
            randomSeed = actualSeed
        }

view: Model -> Html msg
view model =
    let
        (w, h) = (toFloat model.field.width, toFloat model.field.height)

        background = rect w h |> filled black
        block = Block.render model
        structure = Structure.render model

        gameOver = if model.status == GameOver
            then [
                Text.fromString "Game Over"
                    |> Text.typeface ["sans-serif"]
                    |> Text.height 40
                    |> Text.color Color.red
                    |> Collage.text
            ]
            else []

        elems = [background] ++ block ++ structure ++ gameOver
    in
        collage model.field.width model.field.height elems |> toHtml

update: Msg -> Model -> (Model, Cmd Msg)
update action model =
    if model.status == Running then
        case action of
            KeyDown key -> (control key model, Cmd.none)
            Tick dt -> (step dt model, Cmd.none)
    else
        (model, Cmd.none)


step: Float -> Model -> Model
step dt model =
    { model | t = model.t + dt }
        |> fall
        |> checkBlockLanded
        |> Structure.removeCompletedLines -- todo make a separate command for this
        |> Structure.checkGameOver

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
        newRotation = (currentBlock.rotation + 1) % 4
        rotatedBlock = { currentBlock | rotation = newRotation }
    in
        { model | currentBlock = rotatedBlock}

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
        (nextBlock, nextSeed) = Block.random model.randomSeed
        dropNextBlock model = { model |
            currentBlock = model.nextBlock,
            nextBlock = nextBlock,
            x = -1,
            y = startingY model.field nextBlock,
            randomSeed = nextSeed
        }
    in
        if isLanded
        then model |> Structure.fixBlock |> dropNextBlock
        else model

subscriptions: Model -> Sub Msg
subscriptions model = Sub.batch [
    Keyboard.downs KeyDown,
    AnimationFrame.diffs Tick
    ]

main: Program Never
main = App.program {
    init = (init, Cmd.none),
    view = view,
    update = update,
    subscriptions = subscriptions
    }
