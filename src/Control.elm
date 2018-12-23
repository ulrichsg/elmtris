module Control exposing (control)

import Types exposing (..)
import Structure
import Field
import Block exposing (Block)
import Keyboard exposing (Key(..))

control: Key -> Model -> Model
control key model =
    if model.t >= model.lastMove + model.moveInterval
    then
        case key of
            Left -> moveLeft model
            Rotate -> rotate model
            Right -> moveRight model
            Down -> moveDown model
            Other -> model
    else model

moveBlock: (Block -> Block) -> Model -> Model
moveBlock op model =
    let
        newBlock = op model.currentBlock
        newModel = { model | currentBlock = newBlock, lastMove = model.t }
    in
        if Structure.overlaps newBlock newModel.structure || Field.blockOutOfBounds newBlock newModel.field
        then model
        else newModel

moveLeft = moveBlock (\block -> { block | x = block.x - 1 })
moveRight = moveBlock (\block -> { block | x = block.x + 1 })
moveDown = moveBlock (\block -> { block | y = block.y + 1 })
rotate = moveBlock (\block -> { block | rotation = modBy 4 (block.rotation + 1) })
