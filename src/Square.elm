module Square exposing (..)

type Shape = I | O | T | S | Z | L | J

type alias Square = { x: Int, y: Int, size: Int, shape: Shape }

measure: (Square -> Int) -> List Square -> Int
measure dimension squares = case squares of
    [] -> 0
    head :: _ ->
        head.size + List.foldl(\square a -> max (dimension square) a) 0 squares

width = measure (\{x} -> x)
height = measure (\{y} -> y)

--width: List Square -> Int
--width squares =
--    let
--        size = List.head squares |> (\square -> square.size)
--    in
--        size + List.foldl (\{x} a -> max a x) 0 squares
--
--height: List Square -> Int
--height squares =
--    let
--        size = List.head squares |> (\square -> square.size)
--    in
--        size + List.foldl (\{y} a -> max a y) 0 squares

translate: Int -> Int -> List Square -> List Square
translate dx dy = List.map (\square -> { square | x = square.x + dx, y = square.y + dy })

scale: Int -> List Square -> List Square
scale factor = List.map(\square ->
    { square
        | x = square.x * factor
        , y = square.y * factor
        , size = square.size * factor
     })

rotate: Int -> List Square -> List Square
rotate times squares =
    let
        dmirror = List.map (\square -> { square | x = square.y, y = square.x })

        hmirror_ swidth square = { square | x = swidth - square.x }
        hmirror sqrs = List.map (hmirror_ (width sqrs)) sqrs

        vmirror_ sheight square = { square | y = sheight - square.y }
        vmirror sqrs = List.map (vmirror_ (height sqrs)) sqrs
    in
        case times of
            0 -> squares
            1 -> hmirror (dmirror squares)
            2 -> hmirror (vmirror squares)
            3 -> vmirror (dmirror squares)
            _ -> squares