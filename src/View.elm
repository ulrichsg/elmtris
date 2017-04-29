module View exposing (render)

import Html exposing (..)
import Collage exposing (collage, rect, filled, alpha, move, moveX, moveY)
import Element exposing (toHtml)
import Color exposing (..)
import Text exposing (typeface, height, color)

import Types exposing (..)
import Block
import Structure

fonts = ["Tahoma", "Geneva", "Verdana", "Helvetica", "Arial", "Sans-Serif"]

render: Model -> Html msg
render model =
    let
        totalWidth = model.field.width + model.field.sidebarWidth
        (w, h) = (toFloat totalWidth, toFloat model.field.height)
        centerX = toFloat (-1 * model.field.sidebarWidth // 2)

        background = rect w h |> filled black
        block = Block.render model
        structure = Structure.render model

        gameOver = if model.status == GameOver
            then [
                rect (toFloat model.field.width) h
                    |> filled black
                    |> alpha 0.5
                    |> moveX centerX,
                Text.fromString "Game Over"
                    |> typeface fonts
                    |> height 40
                    |> color red
                    |> Collage.text
                    |> moveX centerX
            ]
            else []

        elems = [background] ++ block ++ structure ++ (renderSidebar model) ++ gameOver
    in
        div
            []
            [ collage totalWidth model.field.height elems |> toHtml ]


displayNextBlock model =
    let
        x = model.field.width // 2
        y = model.field.height // 2 - 85
    in
        Block.renderAt model x y model.nextBlock

renderSidebar model =
    let
        sidebarText yPos text =
            Text.fromString text
                |> typeface fonts
                |> height 20
                |> color black
                |> Collage.text
                |> move (toFloat (model.field.width // 2), toFloat (model.field.height // 2 - yPos))
    in [
        rect (toFloat model.field.sidebarWidth) (toFloat model.field.height)
            |> filled darkGray
            |> moveX (toFloat (model.field.width // 2)),
        sidebarText 30 "Next",
        sidebarText 150 "Level",
        sidebarText 175 (toString model.level),
        sidebarText 225 "Lines",
        sidebarText 250 (toString model.lines),
        sidebarText 300 "Score",
        sidebarText 325 (toString model.score),
        sidebarText 375 "High Score",
        sidebarText 400 (toString model.highScore)
    ] ++ displayNextBlock model
