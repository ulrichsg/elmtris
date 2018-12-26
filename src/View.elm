module View exposing (render)

import Canvas exposing (..)
import CanvasColor as Color exposing (Color)
import Html exposing (Html)

import Block exposing (Block)
import Config
import Square exposing (Shape(..), Square)
import Structure
import Model exposing (..)

fonts = "Tahoma, Geneva, Verdana, Helvetica, Arial, Sans-Serif"

displayHeight field = toFloat (field.height * Config.gridSize)
displayWidth field = toFloat (field.width * Config.gridSize)

render : Model -> Html msg
render model =
    let
        totalWidth = model.field.width * Config.gridSize + Config.sidebarWidth
    in
    Canvas.element
        totalWidth
        (model.field.height * Config.gridSize)
        []
        (Canvas.empty
            |> fillStyle Color.black
            |> fillRect 0 0 (toFloat totalWidth) (displayHeight model.field)
            |> renderStructure model
            |> renderCurrentBlock model
            |> renderSidebar model
            |> renderGameOver model
        )

renderStructure : Model -> Commands -> Commands
renderStructure model commands =
    model.structure
        |> Square.scale Config.gridSize
        |> List.foldl drawSquare commands

renderGameOver : Model -> Commands -> Commands
renderGameOver model commands =
    if model.status /= GameOver
    then
        commands
    else
        let
            width = displayWidth model.field
            height = displayHeight model.field
        in
        commands
            |> fillStyle Color.black
            |> globalAlpha 0.5
            |> fillRect 0 0 width height
            |> globalAlpha 1.0
            |> font ("36px " ++ fonts)
            |> fillStyle Color.red
            |> textAlign Center
            |> fillText "Game Over" (width / 2) (height / 2) Nothing

renderSidebar : Model -> Commands -> Commands
renderSidebar model commands =
    let
        width = toFloat Config.sidebarWidth
        height = displayHeight model.field
        leftX = displayWidth model.field
        sidebarText yPos text cmds = fillText text (leftX + width / 2) yPos Nothing <| cmds
    in
    commands
        |> fillStyle Color.darkGray
        |> fillRect leftX 0 width height
        |> font ("24px " ++ fonts)
        |> fillStyle Color.black
        |> textAlign Center
        |> sidebarText 30 "Next"
        |> sidebarText 150 "Level"
        |> sidebarText 175 (String.fromInt model.level)
        |> sidebarText 220 "Lines"
        |> sidebarText 245 (String.fromInt model.lines)
        |> sidebarText 290 "Score"
        |> sidebarText 315 (String.fromInt model.score)
        |> sidebarText 355 "High Score"
        |> sidebarText 380 (String.fromInt model.highScore)
        |> previewBlock model

renderCurrentBlock : Model -> Commands -> Commands
renderCurrentBlock model commands =
    let
        blockSquares = model.currentBlock
                |> Block.toSquares
                |> Square.scale Config.gridSize
    in
    List.foldl drawSquare commands blockSquares

previewBlock : Model -> Commands -> Commands
previewBlock model commands =
    let
        nextBlockWidth = Block.width model.nextBlock * Config.gridSize
        nextBlockHeight = Block.height model.nextBlock * Config.gridSize

        nextBlockX = round (displayWidth model.field) + (Config.sidebarWidth - nextBlockWidth) // 2
        nextBlockY = 75 - nextBlockHeight // 2
    in
    model.nextBlock
        |> Block.toSquares
        |> Square.scale Config.gridSize
        |> Square.translate nextBlockX nextBlockY
        |> List.foldl drawSquare commands

drawSquare : Square -> Commands -> Commands
drawSquare square commands =
    let
        xf = toFloat square.x
        yf = toFloat square.y
        sf = toFloat Config.gridSize
        color = shapeToColor square.shape
    in
    commands
            |> fillStyle color

shapeToColor : Shape -> Color
shapeToColor shape =
    case shape of
        I -> Color.lightBlue
        O -> Color.yellow
        T -> Color.purple
        S -> Color.lightGreen
        Z -> Color.red
        L -> Color.orange
        J -> Color.blue
