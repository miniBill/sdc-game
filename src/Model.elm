module Model exposing
    ( ChatHistory
    , Choice
    , City
    , CityName
    , Condition(..)
    , Consequence(..)
    , Coordinates
    , Data
    , Dialog
    , GameModel(..)
    , Id
    , Item(..)
    , ItemName(..)
    , MapModel
    , MenuModel
    , Nation(..)
    , Next(..)
    , Person
    , Quiz
    , SharedGameModel
    , TalkingModel
    , TransportKind(..)
    , mapSize
    )

import Dict exposing (Dict)
import MapPixels exposing (MapLength)
import Set exposing (Set)



-- Game Model


type alias SharedGameModel =
    { currentPerson : Id
    , tickets : Set Id
    }


type GameModel
    = ViewingMap MapModel
    | ViewingPerson
    | ViewingTalking TalkingModel
    | Quizzing Quiz
    | ViewingMenu MenuModel


type alias MapModel =
    {}


type alias TalkingModel =
    { chatHistory : ChatHistory
    , currentDialog : Dialog
    }


type alias ChatHistory =
    List ( Maybe { image : String, name : String }, String )


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


type Consequence
    = ConsequenceGetMoney Int
    | ConsequenceLoseMoney Int
    | ConsequenceGetItem Item
    | ConsequenceLoseItem String
    | ConsequenceSetLocalFlag String Bool


type Item
    = GenericItem
        { name : String
        , image : String
        }
    | Ticket
        { from : CityName
        , to : CityName
        , kind : TransportKind
        , consequences : List Consequence
        }


type TransportKind
    = Plane
    | Train
    | Coach
    | Bike
    | Boat
    | Ferry
    | DuckWalk


type Condition
    = ConditionNot Condition


type ItemName
    = TicketName
        { from : CityName
        , to : CityName
        , kind : TransportKind
        }


mapSize :
    { width : MapLength
    , height : MapLength
    }
mapSize =
    { width = MapPixels.pixels 1473
    , height = MapPixels.pixels 1198
    }
