module Codecs exposing (choiceCodec, cityCodec, cityNameCodec, conditionCodec, consequenceCodec, coordinatesCodec, dataCodec, dialogCodec, idCodec, itemCodec, itemNameCodec, nationCodec, nextCodec, personCodec, quizCodec, transportKindCodec)

{-| 

@docs dataCodec, idCodec, personCodec, cityCodec, cityNameCodec, coordinatesCodec, nationCodec, dialogCodec, choiceCodec, nextCodec, quizCodec, consequenceCodec, itemCodec, transportKindCodec, conditionCodec, itemNameCodec


-}


import Codec
import Dict
import Model


dataCodec : Codec.Codec Model.Data
dataCodec =
    Codec.dict personCodec


idCodec : Codec.Codec Model.Id
idCodec =
    Codec.string


personCodec : Codec.Codec Model.Person
personCodec =
    Codec.object
     (\name city image dialog quizzes ->
         { name = Maybe.withDefault "" name
         , city = city
         , image = Maybe.withDefault "" image
         , dialog = dialog
         , quizzes = Maybe.withDefault [] quizzes
         }
     )
        |> Codec.maybeField
            "name"
            (\lambdaArg0 ->
                if lambdaArg0.name == "" then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.name
            )
            Codec.string
        |> Codec.field "city" .city cityCodec
        |> Codec.maybeField
            "image"
            (\lambdaArg0 ->
                if lambdaArg0.image == "" then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.image
            )
            Codec.string
        |> Codec.field "dialog" .dialog dialogCodec
        |> Codec.maybeField
            "quizzes"
            (\lambdaArg0 ->
                if lambdaArg0.quizzes == [] then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.quizzes
            )
            (Codec.list quizCodec)
        |> Codec.buildObject


cityCodec : Codec.Codec Model.City
cityCodec =
    Codec.object
     (\name showNameOnTheRightInTheMap text image coordinates nation ->
         { name = name
         , showNameOnTheRightInTheMap =
             Maybe.withDefault True showNameOnTheRightInTheMap
         , text = Maybe.withDefault "" text
         , image = Maybe.withDefault "" image
         , coordinates = coordinates
         , nation = nation
         }
     )
        |> Codec.field "name" .name cityNameCodec
        |> Codec.maybeField
            "showNameOnTheRightInTheMap"
            (\lambdaArg0 ->
                if lambdaArg0.showNameOnTheRightInTheMap == True then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.showNameOnTheRightInTheMap
            )
            Codec.bool
        |> Codec.maybeField
            "text"
            (\lambdaArg0 ->
                if lambdaArg0.text == "" then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.text
            )
            Codec.string
        |> Codec.maybeField
            "image"
            (\lambdaArg0 ->
                if lambdaArg0.image == "" then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.image
            )
            Codec.string
        |> Codec.field "coordinates" .coordinates coordinatesCodec
        |> Codec.field "nation" .nation nationCodec
        |> Codec.buildObject


cityNameCodec : Codec.Codec Model.CityName
cityNameCodec =
    Codec.string


coordinatesCodec : Codec.Codec Model.Coordinates
coordinatesCodec =
    Codec.object
     (\north east ->
         { north = Maybe.withDefault 0 north, east = Maybe.withDefault 0 east }
     )
        |> Codec.maybeField
            "north"
            (\lambdaArg0 ->
                if lambdaArg0.north == 0 then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.north
            )
            Codec.float
        |> Codec.maybeField
            "east"
            (\lambdaArg0 ->
                if lambdaArg0.east == 0 then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.east
            )
            Codec.float
        |> Codec.buildObject


nationCodec : Codec.Codec Model.Nation
nationCodec =
    Codec.lazy <|
        \() ->
            Codec.custom
             (\faustria fbelgium fengland ffrance fgermany fitaly fnetherlands fnorway value ->
                 case value of
                     Model.Austria ->
                         faustria

                     Model.Belgium ->
                         fbelgium

                     Model.England ->
                         fengland

                     Model.France ->
                         ffrance

                     Model.Germany ->
                         fgermany

                     Model.Italy ->
                         fitaly

                     Model.Netherlands ->
                         fnetherlands

                     Model.Norway ->
                         fnorway
             )
                |> Codec.variant0 "Austria" Model.Austria
                |> Codec.variant0 "Belgium" Model.Belgium
                |> Codec.variant0 "England" Model.England
                |> Codec.variant0 "France" Model.France
                |> Codec.variant0 "Germany" Model.Germany
                |> Codec.variant0 "Italy" Model.Italy
                |> Codec.variant0 "Netherlands" Model.Netherlands
                |> Codec.variant0 "Norway" Model.Norway
                |> Codec.buildCustom


dialogCodec : Codec.Codec Model.Dialog
dialogCodec =
    Codec.object
     (\text choices -> { text = Maybe.withDefault "" text, choices = choices })
        |> Codec.maybeField
            "text"
            (\lambdaArg0 ->
                if lambdaArg0.text == "" then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.text
            )
            Codec.string
        |> Codec.field
            "choices"
            .choices
            (Codec.tuple choiceCodec (Codec.list choiceCodec))
        |> Codec.buildObject


choiceCodec : Codec.Codec Model.Choice
choiceCodec =
    Codec.object
     (\text next -> { text = Maybe.withDefault "" text, next = next })
        |> Codec.maybeField
            "text"
            (\lambdaArg0 ->
                if lambdaArg0.text == "" then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.text
            )
            Codec.string
        |> Codec.field "next" .next nextCodec
        |> Codec.buildObject


nextCodec : Codec.Codec Model.Next
nextCodec =
    Codec.lazy <|
        \() ->
            Codec.custom
             (\fnextDialog fnextViewMap fnextRandomQuiz fnextQuiz fnextGiveTicket value ->
                 case value of
                     Model.NextDialog arg0 ->
                         fnextDialog arg0

                     Model.NextViewMap ->
                         fnextViewMap

                     Model.NextRandomQuiz ->
                         fnextRandomQuiz

                     Model.NextQuiz arg0 ->
                         fnextQuiz arg0

                     Model.NextGiveTicket ->
                         fnextGiveTicket
             )
                |> Codec.variant1 "NextDialog" Model.NextDialog dialogCodec
                |> Codec.variant0 "NextViewMap" Model.NextViewMap
                |> Codec.variant0 "NextRandomQuiz" Model.NextRandomQuiz
                |> Codec.variant1 "NextQuiz" Model.NextQuiz quizCodec
                |> Codec.variant0 "NextGiveTicket" Model.NextGiveTicket
                |> Codec.buildCustom


quizCodec : Codec.Codec Model.Quiz
quizCodec =
    Codec.object
     (\question correctAnswer messageIfCorrect messageIfWrong wrongAnswers ->
         { question = Maybe.withDefault "" question
         , correctAnswer = Maybe.withDefault "" correctAnswer
         , messageIfCorrect = Maybe.withDefault "" messageIfCorrect
         , messageIfWrong = Maybe.withDefault "" messageIfWrong
         , wrongAnswers = Maybe.withDefault [] wrongAnswers
         }
     )
        |> Codec.maybeField
            "question"
            (\lambdaArg0 ->
                if lambdaArg0.question == "" then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.question
            )
            Codec.string
        |> Codec.maybeField
            "correctAnswer"
            (\lambdaArg0 ->
                if lambdaArg0.correctAnswer == "" then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.correctAnswer
            )
            Codec.string
        |> Codec.maybeField
            "messageIfCorrect"
            (\lambdaArg0 ->
                if lambdaArg0.messageIfCorrect == "" then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.messageIfCorrect
            )
            Codec.string
        |> Codec.maybeField
            "messageIfWrong"
            (\lambdaArg0 ->
                if lambdaArg0.messageIfWrong == "" then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.messageIfWrong
            )
            Codec.string
        |> Codec.maybeField
            "wrongAnswers"
            (\lambdaArg0 ->
                if lambdaArg0.wrongAnswers == [] then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.wrongAnswers
            )
            (Codec.list Codec.string)
        |> Codec.buildObject


consequenceCodec : Codec.Codec Model.Consequence
consequenceCodec =
    Codec.lazy <|
        \() ->
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
                    (Codec.object
                      (\name image ->
                          { name = Maybe.withDefault "" name
                          , image = Maybe.withDefault "" image
                          }
                      )
                        |> Codec.maybeField
                            "name"
                            (\lambdaArg0 ->
                                if lambdaArg0.name == "" then
                                    Maybe.Nothing

                                else
                                    Maybe.Just lambdaArg0.name
                            )
                            Codec.string
                        |> Codec.maybeField
                            "image"
                            (\lambdaArg0 ->
                                if lambdaArg0.image == "" then
                                    Maybe.Nothing

                                else
                                    Maybe.Just lambdaArg0.image
                            )
                            Codec.string
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
                          , consequences = Maybe.withDefault [] consequences
                          }
                      )
                        |> Codec.field "from" .from cityNameCodec
                        |> Codec.field "to" .to cityNameCodec
                        |> Codec.field "kind" .kind transportKindCodec
                        |> Codec.maybeField
                            "consequences"
                            (\lambdaArg0 ->
                                if lambdaArg0.consequences == [] then
                                    Maybe.Nothing

                                else
                                    Maybe.Just lambdaArg0.consequences
                            )
                            (Codec.list consequenceCodec)
                        |> Codec.buildObject
                    )
                |> Codec.buildCustom


transportKindCodec : Codec.Codec Model.TransportKind
transportKindCodec =
    Codec.lazy <|
        \() ->
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
            Codec.lazy <|
                \() ->
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
                        |> Codec.variant1
                            "ConditionNot"
                            Model.ConditionNot
                            lambdaArg0
                        |> Codec.variant1
                            "ConditionAnd"
                            Model.ConditionAnd
                            (Codec.list lambdaArg0)
                        |> Codec.variant1
                            "ConditionOr"
                            Model.ConditionOr
                            (Codec.list lambdaArg0)
                        |> Codec.variant1 "HasItem" Model.HasItem itemNameCodec
                        |> Codec.variant1
                            "LocalFlag"
                            Model.LocalFlag
                            Codec.string
                        |> Codec.buildCustom
        )


itemNameCodec : Codec.Codec Model.ItemName
itemNameCodec =
    Codec.lazy <|
        \() ->
            Codec.custom
             (\fgenericItemName fticketName value ->
                 case value of
                     Model.GenericItemName arg0 ->
                         fgenericItemName arg0

                     Model.TicketName arg0 ->
                         fticketName arg0
             )
                |> Codec.variant1
                    "GenericItemName"
                    Model.GenericItemName
                    Codec.string
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


