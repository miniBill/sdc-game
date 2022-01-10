module Evergreen.V1.Model exposing (..)

import Dict
import Set


type alias Id =
    String


type alias CityName =
    String


type alias Coordinates =
    { x : Float
    , y : Float
    }


type Nation
    = Austria
    | Belgium
    | England
    | France
    | Germany
    | Italy
    | Netherlands
    | Norway


type alias Sound =
    { name : String
    , duration : Int
    }


type alias City =
    { name : CityName
    , text : String
    , image : String
    , coordinates : Coordinates
    , nation : Nation
    , sound : Sound
    }


type alias Choice =
    { text : String
    , next : Next
    }


type alias Dialog =
    { text : String
    , choices : ( Choice, List Choice )
    }


type alias Quiz =
    { question : String
    , correctAnswer : String
    , messageIfCorrect : String
    , messageIfWrong : String
    , wrongAnswers : List String
    }


type Next
    = NextDialog Dialog
    | NextViewMap
    | NextRandomQuiz
    | NextQuiz Quiz
    | NextGiveTicket


type alias Person =
    { name : String
    , city : City
    , image : String
    , dialog : Dialog
    , quizzes : List Quiz
    }


type alias ChatHistory =
    List
        ( Maybe
            { image : String
            , name : String
            }
        , String
        )


type alias MapModel =
    { travellingTo : Maybe ( Float, Id )
    }


type alias TalkingModel =
    { chatHistory : ChatHistory
    , currentDialog : Dialog
    }


type alias MenuModel =
    { previous : GameModel
    , background : String
    }


type GameModel
    = ViewingMap MapModel
    | ViewingPerson
    | ViewingTalking TalkingModel
    | Quizzing Quiz
    | ViewingMenu MenuModel


type alias Data =
    Dict.Dict Id Person


type alias SharedGameModel =
    { currentPerson : Id
    , tickets : Set.Set Id
    , usedTickets : Set.Set Id
    }
