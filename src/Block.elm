module Block exposing (..)

import Square exposing (Shape(..), Square)

type alias Block =
    { shape : Shape
    , rotation : Int
    , x : Int
    , y : Int
    }

baseSquares block =
    let
        p = \x y -> { x = x, y = y, size = 1, shape = block.shape }
    in
    case block.shape of
        I -> [ p 0 0, p 1 0, p 2 0, p 3 0 ]
        O -> [ p 0 0, p 1 0, p 0 1, p 1 1 ]
        T -> [ p 0 1, p 1 0, p 1 1, p 1 2 ]
        S -> [ p 0 0, p 0 1, p 1 1, p 1 2 ]
        Z -> [ p 0 1, p 0 2, p 1 0, p 1 1 ]
        L -> [ p 0 0, p 1 0, p 0 1, p 0 2 ]
        J -> [ p 0 0, p 1 0, p 1 1, p 1 2 ]

toSquares : Block -> List Square
toSquares block =
    baseSquares block
        |> Square.rotate block.rotation
        |> Square.translate block.x block.y

width block =
    let
        squares = baseSquares block |> Square.rotate block.rotation
    in
    1 + List.foldl (\{ x } a -> max a x) 0 squares

height block =
    let
        squares = baseSquares block |> Square.rotate block.rotation
    in
    1 + List.foldl (\{ y } a -> max a y) 0 squares

bottomY = toSquares >> List.foldl (\{ y } a -> max a y) 0
rightX = toSquares >> List.foldl (\{ x } a -> max a x) 0
