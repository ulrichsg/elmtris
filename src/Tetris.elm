import Html exposing (..)
import Html.App as App
import AnimationFrame
import Keyboard
import Random.Pcg as Random

import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import Text

import Types exposing (..)
import Block
import Structure

type Msg = KeyDown Int | Tick Float | BlockLanded

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
            randomSeed = actualSeed
        }

view: Model -> Html msg
view model =
    let
        totalWidth = model.field.width + model.field.sidebarWidth
        (w, h) = (toFloat totalWidth, toFloat model.field.height)

        background = rect w h |> filled black
        block = Block.render model
        structure = Structure.render model

        gameOver = if model.status == GameOver
            then [
                Text.fromString "Game Over"
                    |> Text.typeface ["sans-serif"]
                    |> Text.height 40
                    |> Text.color red
                    |> Collage.text
                    |> moveX (toFloat (-1 * model.field.sidebarWidth // 2))
            ]
            else []

        sidebarText yPos text =
            Text.fromString text
                |> Text.typeface ["sans-serif"]
                |> Text.height 20
                |> Text.color black
                |> Collage.text
                |> move (toFloat (model.field.width // 2), toFloat (model.field.height // 2 - yPos))

        nextBlock block = []

        sidebar = [
            rect (toFloat model.field.sidebarWidth) (toFloat model.field.height)
                |> filled darkGray
                |> moveX (toFloat (model.field.width // 2)),
            sidebarText 30 "Next",
            sidebarText 150 "Level",
            sidebarText 180 (toString model.level),
            sidebarText 240 "Lines",
            sidebarText 270 (toString model.lines),
            sidebarText 330 "Score",
            sidebarText 360 (toString model.score)
        ] ++ nextBlock model.nextBlock

        elems = [background] ++ block ++ structure ++ sidebar ++ gameOver
    in
        collage totalWidth model.field.height elems |> toHtml

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
        dropNextBlock model = { model |
            currentBlock = model.nextBlock,
            nextBlock = nextBlock,
            x = -1,
            y = startingY model.field nextBlock,
            randomSeed = nextSeed
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
    view = view,
    update = update,
    subscriptions = subscriptions
    }
