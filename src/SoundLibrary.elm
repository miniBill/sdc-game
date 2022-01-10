module SoundLibrary exposing (..)

import Model exposing (Sound)


all : List Sound
all =
    [ click
    , quack1
    , quack2
    , quack3
    , quackquackquack
    , train
    ]


click : Sound
click =
    { name = "click.mp3", duration = 240 }


quack1 : Sound
quack1 =
    { name = "quack1.mp3", duration = 440 }


quack2 : Sound
quack2 =
    { name = "quack2.mp3", duration = 440 }


quack3 : Sound
quack3 =
    { name = "quack3.mp3", duration = 370 }


quackquackquack : Sound
quackquackquack =
    { name = "quackquackquack.mp3", duration = 1180 }


train : Sound
train =
    { name = "train-cut.mp3", duration = 10420 }
