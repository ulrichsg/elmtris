module Main exposing (main)

import Browser
import Browser.Events
import Random

import Config
import Control exposing (control)
import Field
import Keyboard
import Logic exposing (step)
import Model exposing (..)
import View

type alias Flags =
    { highScore : Int
    , randomSeed : Int
    }

init : Flags -> (Model, Cmd Msg)
init flags =
    let
        field =
            { width = Config.fieldWidth
            , height = Config.fieldHeight
            }

        initialSeed = Random.initialSeed flags.randomSeed
        (currentBlock, nextSeed) = Field.spawnBlock initialSeed
        (nextBlock, actualSeed) = Field.spawnBlock nextSeed

        initialModel =
            { status = Running
            , field = field
            , lines = 0
            , level = 1
            , score = 0
            , highScore = flags.highScore
            , currentBlock = currentBlock |> Field.dropBlock field []
            , nextBlock = nextBlock
            , structure = []
            , t = 0.0
            , lastDrop = 0.0
            , dropInterval = Config.initialSpeed
            , lastMove = 0.0
            , moveInterval = Config.sensitivity
            , randomSeed = actualSeed
            }
    in
    (initialModel, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case (model.status, action) of
        (Running, KeyDown key) -> (control key model, Cmd.none)
        (Running, Tick dt) -> step dt model
        _ -> (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.subscribe (\key -> KeyDown key)
        , Browser.Events.onAnimationFrameDelta Tick
        ]

main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = View.render
        , update = update
        , subscriptions = subscriptions
        }
