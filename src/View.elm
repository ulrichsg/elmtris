module View exposing (render)

import Block exposing (Block)
import Canvas exposing (..)
import CanvasColor as Color exposing (Color)
import Html exposing (Html)
import Square exposing (Shape(..), Square)
import Structure
import Types exposing (..)

fonts = "Tahoma, Geneva, Verdana, Helvetica, Arial, Sans-Serif"

render : Model -> Html msg
render model =
    let
        totalWidth = model.field.width + model.field.sidebarWidth
        (w, h) = (toFloat totalWidth, toFloat model.field.height)
    in
    Canvas.element
        totalWidth
        model.field.height
        []
        (Canvas.empty
            |> fillStyle Color.black
            |> fillRect 0 0 w h
            |> renderStructure model
            |> renderCurrentBlock model
            |> renderSidebar model
            |> renderGameOver model
        )

renderStructure : Model -> Commands -> Commands
renderStructure model commands =
    model.structure
        |> Square.scale model.field.gridSize
        |> List.foldl (drawSquare model.field.gridSize) commands

renderGameOver : Model -> Commands -> Commands
renderGameOver model commands =
    if model.status /= GameOver
    then commands
    else
        let
            width = toFloat model.field.width
            height = toFloat model.field.height
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
        width = toFloat model.field.sidebarWidth
        height = toFloat model.field.height
        leftX = toFloat model.field.width
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
                |> Square.scale model.field.gridSize
    in
    List.foldl (drawSquare model.field.gridSize) commands blockSquares

previewBlock : Model -> Commands -> Commands
previewBlock model commands =
    let
        nextBlockWidth = Block.width model.nextBlock * model.field.gridSize
        nextBlockHeight = Block.height model.nextBlock * model.field.gridSize

        nextBlockX = model.field.width + (model.field.sidebarWidth - nextBlockWidth) // 2
        nextBlockY = 75 - nextBlockHeight // 2
    in
    model.nextBlock
        |> Block.toSquares
        |> Square.scale model.field.gridSize
        |> Square.translate nextBlockX nextBlockY
        |> List.foldl (drawSquare model.field.gridSize) commands

drawSquare : Int -> Square -> Commands -> Commands
drawSquare size square commands =
    drawSquareAt square.x square.y size (shapeToColor square.shape) <| commands

drawSquareAt : Int -> Int -> Int -> Color -> Commands -> Commands
drawSquareAt x y size color commands =
    let
        xf = toFloat x
        yf = toFloat y
        sf = toFloat size
    in
    commands
        |> fillStyle color
        |> fillRect xf yf sf sf

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
