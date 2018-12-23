module Types exposing (..)

import Block exposing (Block)
import Field exposing (Field)
import Structure exposing (Structure)
import CanvasColor exposing (Color)
import Random

type Status = Ready | Running | GameOver

type alias Model =
    { status: Status
    , field: Field
    , lines: Int
    , level: Int
    , score: Int
    , highScore: Int
    , currentBlock: Block
    , nextBlock: Block
    , structure: Structure
    , t: Float
    , lastDrop: Float
    , dropInterval: Float
    , lastMove: Float
    , moveInterval: Float
    , randomSeed: Random.Seed
    }
