module Model exposing (City, Data, Id)

import Dict exposing (Dict)


type alias Data =
    Dict Id City


type alias City =
    { name : CityName
    , text : String
    , image : String
    , people : List Person
    }


type alias CityName =
    String


type alias Person =
    { name : String
    , image : String
    , dialog : List ( Id, Dialog )
    }


type alias Id =
    String


type alias Dialog =
    { text : String
    , choices : List Choice
    }


type alias Choice =
    { text : String
    , next : Id
    , consequences : List Consequence
    , condition : Maybe Condition
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
