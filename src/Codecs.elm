module Codecs exposing (..)

import Codec exposing (Codec)
import Model exposing (..)


dataCodec : Codec Data
dataCodec =
    Codec.dict cityCodec


cityCodec : Codec City
cityCodec =
    Codec.object
        (\name text image people ->
            { name = name
            , text = text
            , image = image
            , people = people
            }
        )
        |> Codec.field "name" .name cityNameCodec
        |> Codec.field "text" .text Codec.string
        |> Codec.field "image" .image Codec.string
        |> Codec.field "people" .people (Codec.list personCodec)
        |> Codec.buildObject


cityNameCodec : Codec CityName
cityNameCodec =
    Codec.string


personCodec : Codec Person
personCodec =
    Codec.object
        (\name image dialog ->
            { name = name
            , image = image
            , dialog = dialog
            }
        )
        |> Codec.field "name" .name Codec.string
        |> Codec.field "image" .image Codec.string
        |> Codec.field "dialog" .dialog (Codec.list (Codec.tuple idCodec dialogCodec))
        |> Codec.buildObject


idCodec : Codec Id
idCodec =
    Codec.string


dialogCodec : Codec Dialog
dialogCodec =
    Codec.object
        (\text choices ->
            { text = text
            , choices = choices
            }
        )
        |> Codec.field "text" .text Codec.string
        |> Codec.field "choices" .choices (Codec.list choiceCodec)
        |> Codec.buildObject


choiceCodec : Codec Choice
choiceCodec =
    Codec.object
        (\text next consequences condition ->
            { text = text
            , next = next
            , consequences = consequences
            , condition = condition
            }
        )
        |> Codec.field "text" .text Codec.string
        |> Codec.field "next" .next idCodec
        |> Codec.field "consequences" .consequences (Codec.list consequenceCodec)
        |> Codec.maybeField "condition" .condition conditionCodec
        |> Codec.buildObject


consequenceCodec : Codec Consequence
consequenceCodec =
        Codec.custom
        (\fconsequenceGetMoney fconsequenceLoseMoney fconsequenceGetItem fconsequenceLoseItem fconsequenceSetLocalFlag value ->
            case value of
                ConsequenceGetMoney arg0 ->
                    fconsequenceGetMoney arg0

                ConsequenceLoseMoney arg0 ->
                    fconsequenceLoseMoney arg0

                ConsequenceGetItem arg0 ->
                    fconsequenceGetItem arg0

                ConsequenceLoseItem arg0 ->
                    fconsequenceLoseItem arg0

                ConsequenceSetLocalFlag arg0 arg1 ->
                    fconsequenceSetLocalFlag arg0 arg1
        )
        |> Codec.variant1 "ConsequenceGetMoney" ConsequenceGetMoney Codec.int
        |> Codec.variant1 "ConsequenceLoseMoney" ConsequenceLoseMoney Codec.int
        |> Codec.variant1 "ConsequenceGetItem" ConsequenceGetItem itemCodec
        |> Codec.variant1 "ConsequenceLoseItem" ConsequenceLoseItem Codec.string
        |> Codec.variant2 "ConsequenceSetLocalFlag" ConsequenceSetLocalFlag Codec.string Codec.bool
        |> Codec.buildCustom


itemCodec : Codec Item
itemCodec =
        Codec.custom
        (\fgenericItem fticket value ->
            case value of
                GenericItem arg0 ->
                    fgenericItem arg0

                Ticket arg0 ->
                    fticket arg0
        )
        |> Codec.variant1 "GenericItem" GenericItem (Codec.object
        (\name image ->
            { name = name
            , image = image
            }
        )
        |> Codec.field "name" .name Codec.string
        |> Codec.field "image" .image Codec.string
        |> Codec.buildObject)
        |> Codec.variant1 "Ticket" Ticket (Codec.object
        (\from to kind consequences ->
            { from = from
            , to = to
            , kind = kind
            , consequences = consequences
            }
        )
        |> Codec.field "from" .from cityNameCodec
        |> Codec.field "to" .to cityNameCodec
        |> Codec.field "kind" .kind transportKindCodec
        |> Codec.field "consequences" .consequences (Codec.list consequenceCodec)
        |> Codec.buildObject)
        |> Codec.buildCustom


transportKindCodec : Codec TransportKind
transportKindCodec =
        Codec.custom
        (\fplane ftrain fcoach fbike fboat fferry fduckWalk value ->
            case value of
                Plane ->
                    fplane

                Train ->
                    ftrain

                Coach ->
                    fcoach

                Bike ->
                    fbike

                Boat ->
                    fboat

                Ferry ->
                    fferry

                DuckWalk ->
                    fduckWalk
        )
        |> Codec.variant0 "Plane" Plane
        |> Codec.variant0 "Train" Train
        |> Codec.variant0 "Coach" Coach
        |> Codec.variant0 "Bike" Bike
        |> Codec.variant0 "Boat" Boat
        |> Codec.variant0 "Ferry" Ferry
        |> Codec.variant0 "DuckWalk" DuckWalk
        |> Codec.buildCustom


conditionCodec : Codec Condition
conditionCodec =
    Codec.recursive (\conditionRecursiveCodec ->
            Codec.custom
        (\fconditionNot fconditionAnd fconditionOr fhasItem flocalFlag value ->
            case value of
                ConditionNot arg0 ->
                    fconditionNot arg0

                ConditionAnd arg0 ->
                    fconditionAnd arg0

                ConditionOr arg0 ->
                    fconditionOr arg0

                HasItem arg0 ->
                    fhasItem arg0

                LocalFlag arg0 ->
                    flocalFlag arg0
        )
        |> Codec.variant1 "ConditionNot" ConditionNot conditionRecursiveCodec
        |> Codec.variant1 "ConditionAnd" ConditionAnd (Codec.list conditionRecursiveCodec)
        |> Codec.variant1 "ConditionOr" ConditionOr (Codec.list conditionRecursiveCodec)
        |> Codec.variant1 "HasItem" HasItem itemNameCodec
        |> Codec.variant1 "LocalFlag" LocalFlag Codec.string
        |> Codec.buildCustom    )


itemNameCodec : Codec ItemName
itemNameCodec =
        Codec.custom
        (\fgenericItemName fticketName value ->
            case value of
                GenericItemName arg0 ->
                    fgenericItemName arg0

                TicketName arg0 ->
                    fticketName arg0
        )
        |> Codec.variant1 "GenericItemName" GenericItemName Codec.string
        |> Codec.variant1 "TicketName" TicketName (Codec.object
        (\from to kind ->
            { from = from
            , to = to
            , kind = kind
            }
        )
        |> Codec.field "from" .from cityNameCodec
        |> Codec.field "to" .to cityNameCodec
        |> Codec.field "kind" .kind transportKindCodec
        |> Codec.buildObject)
        |> Codec.buildCustom