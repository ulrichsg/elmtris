module Keyboard exposing (Key(..), decode)

import Json.Decode as Decode

type Key = Left | Right | Down | Rotate | Other

decode: Decode.Decoder Key
decode = Decode.map toKey (Decode.field "key" Decode.string)

toKey: String -> Key
toKey string =
    case string of
        "ArrowLeft" -> Left
        "ArrowRight" -> Right
        "ArrowDown" -> Down
        "ArrowUp" -> Rotate
        _ -> Other
