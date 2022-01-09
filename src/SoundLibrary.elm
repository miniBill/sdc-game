module SoundLibrary exposing (..)

import Types exposing (Sound)


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
    { name = "/art/click.mp3", duration = 240 }


quack1 : Sound
quack1 =
    { name = "/art/quack1.mp3", duration = 440 }


quack2 : Sound
quack2 =
    { name = "/art/quack2.mp3", duration = 440 }


quack3 : Sound
quack3 =
    { name = "/art/quack3.mp3", duration = 370 }


quackquackquack : Sound
quackquackquack =
    { name = "/art/quackquackquack.mp3", duration = 1180 }


train : Sound
train =
    { name = "/art/train.mp3", duration = 50020 }
