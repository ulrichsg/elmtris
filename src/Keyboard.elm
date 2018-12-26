module Keyboard exposing (Key(..), subscribe)

import Browser.Events
import Json.Decode as Decode

type Key = Left | Right | Down | Rotate | Other

subscribe: (Key -> msg) -> Sub msg
subscribe mapping = Browser.Events.onKeyDown (Decode.map mapping decode)

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
