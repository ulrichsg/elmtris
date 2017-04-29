module Control exposing (control)

import Types exposing (..)
import Structure

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
