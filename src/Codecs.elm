module Codecs exposing (sharedGameModelCodec, gameModelCodec, dataCodec, a11yOptionsCodec)

{-|

@docs sharedGameModelCodec, gameModelCodec, dataCodec, a11yOptionsCodec

-}

import Codec
import Model
import Set
import Types exposing (A11yOptions)


a11yOptionsCodec : Codec.Codec A11yOptions
a11yOptionsCodec =
    Codec.object
        (\unlockEverything openDyslexic fontSize opaqueBackgrounds ->
            { unlockEverything = unlockEverything
            , openDyslexic = openDyslexic
            , fontSize = fontSize
            , opaqueBackgrounds = opaqueBackgrounds
            }
        )
        |> Codec.field "unlockEverything" .unlockEverything Codec.bool
        |> Codec.field "openDyslexic" .openDyslexic Codec.bool
        |> Codec.field "fontSize" .fontSize Codec.float
        |> Codec.field "opaqueBackgrounds" .opaqueBackgrounds Codec.bool
        |> Codec.buildObject


sharedGameModelCodec : Codec.Codec Model.SharedGameModel
sharedGameModelCodec =
    Codec.object
        (\currentPerson tickets usedTickets ->
            { currentPerson = currentPerson
            , tickets = Maybe.withDefault Set.empty tickets
            , usedTickets = Maybe.withDefault Set.empty usedTickets
            }
        )
        |> Codec.field "currentPerson" .currentPerson idCodec
        |> Codec.maybeField
            "tickets"
            (\lambdaArg0 ->
                if lambdaArg0.tickets == Set.empty then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.tickets
            )
            (Codec.set idCodec)
        |> Codec.maybeField
            "usedTickets"
            (\lambdaArg0 ->
                if lambdaArg0.usedTickets == Set.empty then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.usedTickets
            )
            (Codec.set idCodec)
        |> Codec.buildObject


gameModelCodec : Codec.Codec Model.GameModel
gameModelCodec =
    Codec.lazy <| \() ->
    Codec.custom
        (\fviewingMap fviewingPerson fviewingTalking fquizzing fviewingMenu value ->
            case value of
                Model.ViewingMap arg0 ->
                    fviewingMap arg0

                Model.ViewingPerson ->
                    fviewingPerson

                Model.ViewingTalking arg0 ->
                    fviewingTalking arg0

                Model.Quizzing arg0 ->
                    fquizzing arg0

                Model.ViewingMenu arg0 ->
                    fviewingMenu arg0
        )
        |> Codec.variant1 "ViewingMap" Model.ViewingMap mapModelCodec
        |> Codec.variant0 "ViewingPerson" Model.ViewingPerson
        |> Codec.variant1
            "ViewingTalking"
            Model.ViewingTalking
            talkingModelCodec
        |> Codec.variant1 "Quizzing" Model.Quizzing quizCodec
        |> Codec.variant1 "ViewingMenu" Model.ViewingMenu menuModelCodec
        |> Codec.buildCustom


mapModelCodec : Codec.Codec Model.MapModel
mapModelCodec =
    Codec.object
        (\travellingTo -> { travellingTo = travellingTo })
        |> Codec.field "travellingTo" .travellingTo (Codec.maybe (Codec.tuple Codec.float idCodec))
        |> Codec.buildObject


talkingModelCodec : Codec.Codec Model.TalkingModel
talkingModelCodec =
    Codec.object
        (\chatHistory currentDialog ->
            { chatHistory = chatHistory, currentDialog = currentDialog }
        )
        |> Codec.field "chatHistory" .chatHistory chatHistoryCodec
        |> Codec.field "currentDialog" .currentDialog dialogCodec
        |> Codec.buildObject


chatHistoryCodec : Codec.Codec Model.ChatHistory
chatHistoryCodec =
    Codec.list
        (Codec.tuple
            (Codec.maybe
                (Codec.object
                    (\image name ->
                        { image = Maybe.withDefault "" image
                        , name = Maybe.withDefault "" name
                        }
                    )
                    |> Codec.maybeField
                        "image"
                        (\lambdaArg0 ->
                            if lambdaArg0.image == "" then
                                Maybe.Nothing

                            else
                                Maybe.Just lambdaArg0.image
                        )
                        Codec.string
                    |> Codec.maybeField
                        "name"
                        (\lambdaArg0 ->
                            if lambdaArg0.name == "" then
                                Maybe.Nothing

                            else
                                Maybe.Just lambdaArg0.name
                        )
                        Codec.string
                    |> Codec.buildObject
                )
            )
            Codec.string
        )


menuModelCodec : Codec.Codec Model.MenuModel
menuModelCodec =
    Codec.object
        (\previous background ->
            { previous = previous, background = Maybe.withDefault "" background }
        )
        |> Codec.field "previous" .previous gameModelCodec
        |> Codec.maybeField
            "background"
            (\lambdaArg0 ->
                if lambdaArg0.background == "" then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.background
            )
            Codec.string
        |> Codec.buildObject


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
        (\name text image coordinates nation sound ->
            { name = name
            , text = Maybe.withDefault "" text
            , image = Maybe.withDefault "" image
            , coordinates = coordinates
            , nation = nation
            , sound = sound
            }
        )
        |> Codec.field "name" .name cityNameCodec
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
        |> Codec.field "sound" .sound soundCodec
        |> Codec.buildObject


soundCodec : Codec.Codec Model.Sound
soundCodec =
    Codec.object
        (\name duration ->
            { name = name
            , duration = duration
            }
        )
        |> Codec.field "name" .name Codec.string
        |> Codec.field "duration" .duration Codec.int
        |> Codec.buildObject


cityNameCodec : Codec.Codec Model.CityName
cityNameCodec =
    Codec.string


coordinatesCodec : Codec.Codec Model.Coordinates
coordinatesCodec =
    Codec.object
        (\x y -> { x = Maybe.withDefault 0 x, y = Maybe.withDefault 0 y })
        |> Codec.maybeField
            "x"
            (\lambdaArg0 ->
                if lambdaArg0.x == 0 then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.x
            )
            Codec.float
        |> Codec.maybeField
            "y"
            (\lambdaArg0 ->
                if lambdaArg0.y == 0 then
                    Maybe.Nothing

                else
                    Maybe.Just lambdaArg0.y
            )
            Codec.float
        |> Codec.buildObject


nationCodec : Codec.Codec Model.Nation
nationCodec =
    Codec.lazy <| \() ->
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
    Codec.lazy <| \() ->
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
    Codec.lazy <| \() ->
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
    Codec.lazy <| \() ->
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
    Codec.lazy <| \() ->
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
