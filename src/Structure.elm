module Structure exposing (..)

import Square exposing (Square)
import Block exposing (Block)

import Array
import Maybe

type alias Structure = List Square

fixBlock: Block -> Structure -> Structure
fixBlock block structure = List.append (Block.toSquares block) structure

tryAll f structure squares =
    let
        tryOne str square = List.any (f square) str
    in
        List.any (tryOne structure) squares

touchesFromAbove: Block -> Structure -> Bool
touchesFromAbove block structure =
    let
        touchesFromAbove_ square1 square2 = square1.x == square2.x && square1.y == square2.y - 1
    in
        tryAll touchesFromAbove_ structure (Block.toSquares block)

overlaps: Block -> Structure -> Bool
overlaps block structure =
    let
        equals square1 square2 = square1.x == square2.x && square1.y == square2.y
    in
        tryAll equals structure (Block.toSquares block)
