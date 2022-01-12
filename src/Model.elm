module Model exposing
    ( A11yOptions
    , ChatHistory
    , ChatLine
    , Choice
    , City
    , CityName
    , Coordinates
    , Data
    , Dialog
    , GameModel(..)
    , Id
    , MapModel
    , MenuModel
    , Nation(..)
    , Next(..)
    , Person
    , Quiz
    , SharedGameModel
    , Sound
    , TalkingModel
    , mapSize
    )

import Dict exposing (Dict)
import MapPixels exposing (MapLength)
import Set exposing (Set)



-- Game Model


type alias A11yOptions =
    { unlockEverything : Bool
    , openDyslexic : Bool
    , fontSize : Float
    , opaqueBackgrounds : Bool
    }


type alias SharedGameModel =
    { currentPerson : Id
    , tickets : Set Id
    , usedTickets : Set Id
    }


type GameModel
    = ViewingMap MapModel
    | ViewingPerson
    | ViewingTalking TalkingModel
    | Quizzing Quiz
    | ViewingMenu MenuModel


type alias MapModel =
    { travellingTo : Maybe ( Float, Id )
    }


type alias TalkingModel =
    { chatHistory : ChatHistory
    , currentDialog : Dialog
    }


type alias ChatHistory =
    List ChatLine


type alias ChatLine =
    { image : String
    , name : String
    , line : String
    }


type alias MenuModel =
    { previous : GameModel
    , background : String
    }



-- Backend data


type alias Data =
    Dict Id Person


type alias Id =
    String


type alias Person =
    { name : String
    , city : City
    , image : String
    , dialog : Dialog
    , quizzes : List Quiz
    }


type alias City =
    { name : CityName
    , text : String
    , image : String
    , coordinates : Coordinates
    , nation : Nation
    , sound : Sound
    }


type alias Sound =
    { name : String
    , duration : Int -- In milliseconds
    }


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


type alias Dialog =
    { text : String
    , choices : ( Choice, List Choice )
    }


type alias Choice =
    { text : String
    , next : Next
    }


type Next
    = NextDialog Dialog
    | NextViewMap
    | NextRandomQuiz
    | NextQuiz Quiz
    | NextGiveTicket


type alias Quiz =
    { question : String
    , correctAnswer : String
    , messageIfCorrect : String
    , messageIfWrong : String
    , wrongAnswers : List String
    }


mapSize :
    { width : MapLength
    , height : MapLength
    }
mapSize =
    { width = MapPixels.pixels 1473
    , height = MapPixels.pixels 1198
    }
