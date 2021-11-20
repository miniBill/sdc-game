module Model exposing
    ( Choice
    , City
    , CityName
    , Condition(..)
    , Consequence(..)
    , Coordinates
    , Data
    , Dialog
    , Id
    , Item(..)
    , ItemName(..)
    , Nation(..)
    , Next(..)
    , Person
    , Quiz
    , TransportKind(..)
    )

import Dict exposing (Dict)


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
    { north : Float
    , east : Float
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
    | ConditionAnd (List Condition)
    | ConditionOr (List Condition)
    | HasItem ItemName
    | LocalFlag String


type ItemName
    = GenericItemName String
    | TicketName
        { from : CityName
        , to : CityName
        , kind : TransportKind
        }
