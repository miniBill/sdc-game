module Codecs exposing (dataCodec)

{-|

@docs dataCodec

-}

import Codec
import Model


dataCodec : Codec.Codec Model.Data
dataCodec =
    Codec.dict cityCodec


cityCodec : Codec.Codec Model.City
cityCodec =
    Codec.object
        (\name text image people ->
            { name = name, text = text, image = image, people = people }
        )
        |> Codec.field "name" .name cityNameCodec
        |> Codec.field "text" .text Codec.string
        |> Codec.field "image" .image Codec.string
        |> Codec.field "people" .people (Codec.list personCodec)
        |> Codec.buildObject


cityNameCodec : Codec.Codec Model.CityName
cityNameCodec =
    Codec.string


personCodec : Codec.Codec Model.Person
personCodec =
    Codec.object
        (\name image dialog -> { name = name, image = image, dialog = dialog })
        |> Codec.field "name" .name Codec.string
        |> Codec.field "image" .image Codec.string
        |> Codec.field
            "dialog"
            .dialog
            (Codec.list (Codec.tuple idCodec dialogCodec))
        |> Codec.buildObject


idCodec : Codec.Codec Model.Id
idCodec =
    Codec.string


dialogCodec : Codec.Codec Model.Dialog
dialogCodec =
    Codec.object (\text choices -> { text = text, choices = choices })
        |> Codec.field "text" .text Codec.string
        |> Codec.field "choices" .choices (Codec.list choiceCodec)
        |> Codec.buildObject


choiceCodec : Codec.Codec Model.Choice
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
        |> Codec.field
            "consequences"
            .consequences
            (Codec.list consequenceCodec)
        |> Codec.maybeField "condition" .condition conditionCodec
        |> Codec.buildObject


consequenceCodec : Codec.Codec Model.Consequence
consequenceCodec =
    Codec.custom
        (\fconsequenceGetMoney fconsequenceLoseMoney fconsequenceGetItem fconsequenceLoseItem fconsequenceSetLocalFlag value ->
            case value of
                Model.ConsequenceGetMoney arg0 ->
                    fconsequenceGetMoney arg0

                Model.ConsequenceLoseMoney arg0 ->
                    fconsequenceLoseMoney arg0

                Model.ConsequenceGetItem arg0 ->
                    fconsequenceGetItem arg0

                Model.ConsequenceLoseItem arg0 ->
                    fconsequenceLoseItem arg0

                Model.ConsequenceSetLocalFlag arg0 arg1 ->
                    fconsequenceSetLocalFlag arg0 arg1
        )
        |> Codec.variant1
            "ConsequenceGetMoney"
            Model.ConsequenceGetMoney
            Codec.int
        |> Codec.variant1
            "ConsequenceLoseMoney"
            Model.ConsequenceLoseMoney
            Codec.int
        |> Codec.variant1
            "ConsequenceGetItem"
            Model.ConsequenceGetItem
            itemCodec
        |> Codec.variant1
            "ConsequenceLoseItem"
            Model.ConsequenceLoseItem
            Codec.string
        |> Codec.variant2
            "ConsequenceSetLocalFlag"
            Model.ConsequenceSetLocalFlag
            Codec.string
            Codec.bool
        |> Codec.buildCustom


itemCodec : Codec.Codec Model.Item
itemCodec =
    Codec.lazy <|
        \() ->
            Codec.custom
                (\fgenericItem fticket value ->
                    case value of
                        Model.GenericItem arg0 ->
                            fgenericItem arg0

                        Model.Ticket arg0 ->
                            fticket arg0
                )
                |> Codec.variant1
                    "GenericItem"
                    Model.GenericItem
                    (Codec.object (\name image -> { name = name, image = image })
                        |> Codec.field "name" .name Codec.string
                        |> Codec.field "image" .image Codec.string
                        |> Codec.buildObject
                    )
                |> Codec.variant1
                    "Ticket"
                    Model.Ticket
                    (Codec.object
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
                        |> Codec.field
                            "consequences"
                            .consequences
                            (Codec.list consequenceCodec)
                        |> Codec.buildObject
                    )
                |> Codec.buildCustom


transportKindCodec : Codec.Codec Model.TransportKind
transportKindCodec =
    Codec.custom
        (\fplane ftrain fcoach fbike fboat fferry fduckWalk value ->
            case value of
                Model.Plane ->
                    fplane

                Model.Train ->
                    ftrain

                Model.Coach ->
                    fcoach

                Model.Bike ->
                    fbike

                Model.Boat ->
                    fboat

                Model.Ferry ->
                    fferry

                Model.DuckWalk ->
                    fduckWalk
        )
        |> Codec.variant0 "Plane" Model.Plane
        |> Codec.variant0 "Train" Model.Train
        |> Codec.variant0 "Coach" Model.Coach
        |> Codec.variant0 "Bike" Model.Bike
        |> Codec.variant0 "Boat" Model.Boat
        |> Codec.variant0 "Ferry" Model.Ferry
        |> Codec.variant0 "DuckWalk" Model.DuckWalk
        |> Codec.buildCustom


conditionCodec : Codec.Codec Model.Condition
conditionCodec =
    Codec.recursive
        (\lambdaArg0 ->
            Codec.custom
                (\fconditionNot fconditionAnd fconditionOr fhasItem flocalFlag value ->
                    case value of
                        Model.ConditionNot arg0 ->
                            fconditionNot arg0

                        Model.ConditionAnd arg0 ->
                            fconditionAnd arg0

                        Model.ConditionOr arg0 ->
                            fconditionOr arg0

                        Model.HasItem arg0 ->
                            fhasItem arg0

                        Model.LocalFlag arg0 ->
                            flocalFlag arg0
                )
                |> Codec.variant1 "ConditionNot" Model.ConditionNot lambdaArg0
                |> Codec.variant1
                    "ConditionAnd"
                    Model.ConditionAnd
                    (Codec.list lambdaArg0)
                |> Codec.variant1
                    "ConditionOr"
                    Model.ConditionOr
                    (Codec.list lambdaArg0)
                |> Codec.variant1 "HasItem" Model.HasItem itemNameCodec
                |> Codec.variant1 "LocalFlag" Model.LocalFlag Codec.string
                |> Codec.buildCustom
        )


itemNameCodec : Codec.Codec Model.ItemName
itemNameCodec =
    Codec.custom
        (\fgenericItemName fticketName value ->
            case value of
                Model.GenericItemName arg0 ->
                    fgenericItemName arg0

                Model.TicketName arg0 ->
                    fticketName arg0
        )
        |> Codec.variant1 "GenericItemName" Model.GenericItemName Codec.string
        |> Codec.variant1
            "TicketName"
            Model.TicketName
            (Codec.object
                (\from to kind -> { from = from, to = to, kind = kind })
                |> Codec.field "from" .from cityNameCodec
                |> Codec.field "to" .to cityNameCodec
                |> Codec.field "kind" .kind transportKindCodec
                |> Codec.buildObject
            )
        |> Codec.buildCustom
