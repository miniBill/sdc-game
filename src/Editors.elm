module Editors exposing (a11yOptionsEditor, sharedGameModelEditor, gameModelEditor, mapModelEditor, talkingModelEditor, chatHistoryEditor, chatLineEditor, menuModelEditor, dataEditor, idEditor, personEditor, cityEditor, soundEditor, cityNameEditor, coordinatesEditor, nationEditor, dialogEditor, choiceEditor, nextEditor, quizEditor, a11yOptionsDefault, sharedGameModelDefault, gameModelDefault, mapModelDefault, talkingModelDefault, chatHistoryDefault, chatLineDefault, menuModelDefault, dataDefault, idDefault, personDefault, cityDefault, soundDefault, cityNameDefault, coordinatesDefault, nationDefault, dialogDefault, choiceDefault, nextDefault, quizDefault)

{-|

@docs a11yOptionsEditor, sharedGameModelEditor, gameModelEditor, mapModelEditor, talkingModelEditor, chatHistoryEditor, chatLineEditor, menuModelEditor, dataEditor, idEditor, personEditor, cityEditor, soundEditor, cityNameEditor, coordinatesEditor, nationEditor, dialogEditor, choiceEditor, nextEditor, quizEditor, a11yOptionsDefault, sharedGameModelDefault, gameModelDefault, mapModelDefault, talkingModelDefault, chatHistoryDefault, chatLineDefault, menuModelDefault, dataDefault, idDefault, personDefault, cityDefault, soundDefault, cityNameDefault, coordinatesDefault, nationDefault, dialogDefault, choiceDefault, nextDefault, quizDefault

-}

import Dict
import Frontend.EditorTheme
import Model
import Set


a11yOptionsEditor : Int -> Model.A11yOptions -> Frontend.EditorTheme.Element Model.A11yOptions
a11yOptionsEditor level value =
    let
        rawSimples =
            [ ( "Unlock everything"
              , Frontend.EditorTheme.map
                    (\f -> { value | unlockEverything = f })
                    (Frontend.EditorTheme.boolEditor
                        (level + 1)
                        value.unlockEverything
                    )
              )
            , ( "Open dyslexic"
              , Frontend.EditorTheme.map
                    (\f -> { value | openDyslexic = f })
                    (Frontend.EditorTheme.boolEditor
                        (level + 1)
                        value.openDyslexic
                    )
              )
            , ( "Font size"
              , Frontend.EditorTheme.map
                    (\f -> { value | fontSize = f })
                    (Frontend.EditorTheme.floatEditor (level + 1) value.fontSize)
              )
            , ( "Opaque backgrounds"
              , Frontend.EditorTheme.map
                    (\f -> { value | opaqueBackgrounds = f })
                    (Frontend.EditorTheme.boolEditor
                        (level + 1)
                        value.opaqueBackgrounds
                    )
              )
            ]

        rawComplexes =
            []
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


sharedGameModelEditor :
    Int
    -> Model.SharedGameModel
    -> Frontend.EditorTheme.Element Model.SharedGameModel
sharedGameModelEditor level value =
    let
        rawSimples =
            [ ( "Current person"
              , Frontend.EditorTheme.map
                    (\f -> { value | currentPerson = f })
                    (idEditor (level + 1) value.currentPerson)
              )
            ]

        rawComplexes =
            [ ( "Tickets"
              , Frontend.EditorTheme.map
                    (\f -> { value | tickets = f })
                    (Frontend.EditorTheme.setEditor
                        "Id"
                        idEditor
                        idDefault
                        (level + 1)
                        value.tickets
                    )
              )
            , ( "Used tickets"
              , Frontend.EditorTheme.map
                    (\f -> { value | usedTickets = f })
                    (Frontend.EditorTheme.setEditor
                        "Id"
                        idEditor
                        idDefault
                        (level + 1)
                        value.usedTickets
                    )
              )
            ]
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


gameModelEditor : Int -> Model.GameModel -> Frontend.EditorTheme.Element Model.GameModel
gameModelEditor level value =
    let
        extractedDefault =
            { mapModelExtracted = mapModelDefault
            , menuModelExtracted = menuModelDefault
            , quizExtracted = quizDefault
            , talkingModelExtracted = talkingModelDefault
            }

        { mapModelExtracted, menuModelExtracted, quizExtracted, talkingModelExtracted } =
            case value of
                Model.ViewingMap mapModel ->
                    { extractedDefault | mapModelExtracted = mapModel }

                Model.ViewingPerson ->
                    extractedDefault

                Model.ViewingTalking talkingModel ->
                    { extractedDefault | talkingModelExtracted = talkingModel }

                Model.Quizzing quiz ->
                    { extractedDefault | quizExtracted = quiz }

                Model.ViewingMenu menuModel ->
                    { extractedDefault | menuModelExtracted = menuModel }

        variants =
            [ ( "Viewing map", Model.ViewingMap mapModelExtracted )
            , ( "Viewing person", Model.ViewingPerson )
            , ( "Viewing talking", Model.ViewingTalking talkingModelExtracted )
            , ( "Quizzing", Model.Quizzing quizExtracted )
            , ( "Viewing menu", Model.ViewingMenu menuModelExtracted )
            ]

        inputsRow =
            case value of
                Model.ViewingMap mapModel ->
                    [ Frontend.EditorTheme.map
                        (\f -> Model.ViewingMap f)
                        (mapModelEditor (level + 1) mapModel)
                    ]

                Model.ViewingPerson ->
                    []

                Model.ViewingTalking talkingModel ->
                    [ Frontend.EditorTheme.map
                        (\f -> Model.ViewingTalking f)
                        (talkingModelEditor (level + 1) talkingModel)
                    ]

                Model.Quizzing quiz ->
                    [ Frontend.EditorTheme.map
                        (\f -> Model.Quizzing f)
                        (quizEditor (level + 1) quiz)
                    ]

                Model.ViewingMenu menuModel ->
                    [ Frontend.EditorTheme.map
                        (\f -> Model.ViewingMenu f)
                        (menuModelEditor (level + 1) menuModel)
                    ]
    in
    Frontend.EditorTheme.customEditor variants inputsRow level value


mapModelEditor : Int -> Model.MapModel -> Frontend.EditorTheme.Element Model.MapModel
mapModelEditor level value =
    let
        rawSimples =
            []

        rawComplexes =
            [ ( "Travelling to"
              , Frontend.EditorTheme.map
                    (\f -> { value | travellingTo = f })
                    (Frontend.EditorTheme.maybeEditor
                        "(Float, Id)"
                        (Frontend.EditorTheme.tupleEditor
                            Frontend.EditorTheme.floatEditor
                            True
                            idEditor
                            True
                        )
                        ( 0, idDefault )
                        (level + 1)
                        value.travellingTo
                    )
              )
            ]
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


talkingModelEditor : Int -> Model.TalkingModel -> Frontend.EditorTheme.Element Model.TalkingModel
talkingModelEditor level value =
    let
        rawSimples =
            []

        rawComplexes =
            [ ( "Chat history"
              , Frontend.EditorTheme.map
                    (\f -> { value | chatHistory = f })
                    (chatHistoryEditor (level + 1) value.chatHistory)
              )
            , ( "Current dialog"
              , Frontend.EditorTheme.map
                    (\f -> { value | currentDialog = f })
                    (dialogEditor (level + 1) value.currentDialog)
              )
            ]
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


chatHistoryEditor : Int -> Model.ChatHistory -> Frontend.EditorTheme.Element Model.ChatHistory
chatHistoryEditor level value =
    Frontend.EditorTheme.listEditor
        "ChatLine"
        chatLineEditor
        chatLineDefault
        level
        value


chatLineEditor : Int -> Model.ChatLine -> Frontend.EditorTheme.Element Model.ChatLine
chatLineEditor level value =
    let
        rawSimples =
            [ ( "Image"
              , Frontend.EditorTheme.map
                    (\f -> { value | image = f })
                    (Frontend.EditorTheme.stringEditor (level + 1) value.image)
              )
            , ( "Name"
              , Frontend.EditorTheme.map
                    (\f -> { value | name = f })
                    (Frontend.EditorTheme.stringEditor (level + 1) value.name)
              )
            , ( "Line"
              , Frontend.EditorTheme.map
                    (\f -> { value | line = f })
                    (Frontend.EditorTheme.stringEditor (level + 1) value.line)
              )
            ]

        rawComplexes =
            []
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


menuModelEditor : Int -> Model.MenuModel -> Frontend.EditorTheme.Element Model.MenuModel
menuModelEditor level value =
    let
        rawSimples =
            [ ( "Background"
              , Frontend.EditorTheme.map
                    (\f -> { value | background = f })
                    (Frontend.EditorTheme.stringEditor
                        (level + 1)
                        value.background
                    )
              )
            ]

        rawComplexes =
            [ ( "Previous"
              , Frontend.EditorTheme.map
                    (\f -> { value | previous = f })
                    (gameModelEditor (level + 1) value.previous)
              )
            ]
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


dataEditor : Int -> Model.Data -> Frontend.EditorTheme.Element Model.Data
dataEditor level value =
    Frontend.EditorTheme.dictEditor
        idEditor
        idDefault
        personEditor
        personDefault
        level
        value


idEditor : Int -> Model.Id -> Frontend.EditorTheme.Element Model.Id
idEditor level value =
    Frontend.EditorTheme.stringEditor level value


personEditor : Int -> Model.Person -> Frontend.EditorTheme.Element Model.Person
personEditor level value =
    let
        rawSimples =
            [ ( "Name"
              , Frontend.EditorTheme.map
                    (\f -> { value | name = f })
                    (Frontend.EditorTheme.stringEditor (level + 1) value.name)
              )
            , ( "Image"
              , Frontend.EditorTheme.map
                    (\f -> { value | image = f })
                    (Frontend.EditorTheme.stringEditor (level + 1) value.image)
              )
            ]

        rawComplexes =
            [ ( "City"
              , Frontend.EditorTheme.map
                    (\f -> { value | city = f })
                    (cityEditor (level + 1) value.city)
              )
            , ( "Dialog"
              , Frontend.EditorTheme.map
                    (\f -> { value | dialog = f })
                    (dialogEditor (level + 1) value.dialog)
              )
            , ( "Quizzes"
              , Frontend.EditorTheme.map
                    (\f -> { value | quizzes = f })
                    (Frontend.EditorTheme.listEditor
                        "Quiz"
                        quizEditor
                        quizDefault
                        (level + 1)
                        value.quizzes
                    )
              )
            ]
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


cityEditor : Int -> Model.City -> Frontend.EditorTheme.Element Model.City
cityEditor level value =
    let
        rawSimples =
            [ ( "Name"
              , Frontend.EditorTheme.map
                    (\f -> { value | name = f })
                    (cityNameEditor (level + 1) value.name)
              )
            , ( "Text"
              , Frontend.EditorTheme.map
                    (\f -> { value | text = f })
                    (Frontend.EditorTheme.stringEditor (level + 1) value.text)
              )
            , ( "Image"
              , Frontend.EditorTheme.map
                    (\f -> { value | image = f })
                    (Frontend.EditorTheme.stringEditor (level + 1) value.image)
              )
            ]

        rawComplexes =
            [ ( "Coordinates"
              , Frontend.EditorTheme.map
                    (\f -> { value | coordinates = f })
                    (coordinatesEditor (level + 1) value.coordinates)
              )
            , ( "Nation"
              , Frontend.EditorTheme.map
                    (\f -> { value | nation = f })
                    (nationEditor (level + 1) value.nation)
              )
            , ( "Sound"
              , Frontend.EditorTheme.map
                    (\f -> { value | sound = f })
                    (soundEditor (level + 1) value.sound)
              )
            ]
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


soundEditor : Int -> Model.Sound -> Frontend.EditorTheme.Element Model.Sound
soundEditor level value =
    let
        rawSimples =
            [ ( "Name"
              , Frontend.EditorTheme.map
                    (\f -> { value | name = f })
                    (Frontend.EditorTheme.stringEditor (level + 1) value.name)
              )
            , ( "Duration"
              , Frontend.EditorTheme.map
                    (\f -> { value | duration = f })
                    (Frontend.EditorTheme.intEditor (level + 1) value.duration)
              )
            ]

        rawComplexes =
            []
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


cityNameEditor : Int -> Model.CityName -> Frontend.EditorTheme.Element Model.CityName
cityNameEditor level value =
    Frontend.EditorTheme.stringEditor level value


coordinatesEditor : Int -> Model.Coordinates -> Frontend.EditorTheme.Element Model.Coordinates
coordinatesEditor level value =
    let
        rawSimples =
            [ ( "X"
              , Frontend.EditorTheme.map
                    (\f -> { value | x = f })
                    (Frontend.EditorTheme.floatEditor (level + 1) value.x)
              )
            , ( "Y"
              , Frontend.EditorTheme.map
                    (\f -> { value | y = f })
                    (Frontend.EditorTheme.floatEditor (level + 1) value.y)
              )
            ]

        rawComplexes =
            []
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


nationEditor : Int -> Model.Nation -> Frontend.EditorTheme.Element Model.Nation
nationEditor level value =
    let
        variants =
            [ ( "Austria", Model.Austria )
            , ( "Belgium", Model.Belgium )
            , ( "England", Model.England )
            , ( "France", Model.France )
            , ( "Germany", Model.Germany )
            , ( "Italy", Model.Italy )
            , ( "Netherlands", Model.Netherlands )
            , ( "Norway", Model.Norway )
            ]
    in
    Frontend.EditorTheme.enumEditor variants value level


dialogEditor : Int -> Model.Dialog -> Frontend.EditorTheme.Element Model.Dialog
dialogEditor level value =
    let
        rawSimples =
            [ ( "Text"
              , Frontend.EditorTheme.map
                    (\f -> { value | text = f })
                    (Frontend.EditorTheme.stringEditor (level + 1) value.text)
              )
            ]

        rawComplexes =
            [ ( "Choices"
              , Frontend.EditorTheme.map
                    (\f -> { value | choices = f })
                    (Frontend.EditorTheme.tupleEditor
                        choiceEditor
                        False
                        (Frontend.EditorTheme.listEditor
                            "Choice"
                            choiceEditor
                            choiceDefault
                        )
                        False
                        (level + 1)
                        value.choices
                    )
              )
            ]
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


choiceEditor : Int -> Model.Choice -> Frontend.EditorTheme.Element Model.Choice
choiceEditor level value =
    let
        rawSimples =
            [ ( "Text"
              , Frontend.EditorTheme.map
                    (\f -> { value | text = f })
                    (Frontend.EditorTheme.stringEditor (level + 1) value.text)
              )
            ]

        rawComplexes =
            [ ( "Next"
              , Frontend.EditorTheme.map
                    (\f -> { value | next = f })
                    (nextEditor (level + 1) value.next)
              )
            ]
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


nextEditor : Int -> Model.Next -> Frontend.EditorTheme.Element Model.Next
nextEditor level value =
    let
        extractedDefault =
            { dialogExtracted = dialogDefault, quizExtracted = quizDefault }

        { dialogExtracted, quizExtracted } =
            case value of
                Model.NextDialog dialog ->
                    { extractedDefault | dialogExtracted = dialog }

                Model.NextViewMap ->
                    extractedDefault

                Model.NextRandomQuiz ->
                    extractedDefault

                Model.NextQuiz quiz ->
                    { extractedDefault | quizExtracted = quiz }

                Model.NextGiveTicket ->
                    extractedDefault

        variants =
            [ ( "Dialog", Model.NextDialog dialogExtracted )
            , ( "View map", Model.NextViewMap )
            , ( "Random quiz", Model.NextRandomQuiz )
            , ( "Quiz", Model.NextQuiz quizExtracted )
            , ( "Give ticket", Model.NextGiveTicket )
            ]

        inputsRow =
            case value of
                Model.NextDialog dialog ->
                    [ Frontend.EditorTheme.map
                        (\f -> Model.NextDialog f)
                        (dialogEditor (level + 1) dialog)
                    ]

                Model.NextViewMap ->
                    []

                Model.NextRandomQuiz ->
                    []

                Model.NextQuiz quiz ->
                    [ Frontend.EditorTheme.map
                        (\f -> Model.NextQuiz f)
                        (quizEditor (level + 1) quiz)
                    ]

                Model.NextGiveTicket ->
                    []
    in
    Frontend.EditorTheme.customEditor variants inputsRow level value


quizEditor : Int -> Model.Quiz -> Frontend.EditorTheme.Element Model.Quiz
quizEditor level value =
    let
        rawSimples =
            [ ( "Question"
              , Frontend.EditorTheme.map
                    (\f -> { value | question = f })
                    (Frontend.EditorTheme.stringEditor
                        (level + 1)
                        value.question
                    )
              )
            , ( "Correct answer"
              , Frontend.EditorTheme.map
                    (\f -> { value | correctAnswer = f })
                    (Frontend.EditorTheme.stringEditor
                        (level + 1)
                        value.correctAnswer
                    )
              )
            , ( "Message if correct"
              , Frontend.EditorTheme.map
                    (\f -> { value | messageIfCorrect = f })
                    (Frontend.EditorTheme.stringEditor
                        (level + 1)
                        value.messageIfCorrect
                    )
              )
            , ( "Message if wrong"
              , Frontend.EditorTheme.map
                    (\f -> { value | messageIfWrong = f })
                    (Frontend.EditorTheme.stringEditor
                        (level + 1)
                        value.messageIfWrong
                    )
              )
            ]

        rawComplexes =
            [ ( "Wrong answers"
              , Frontend.EditorTheme.map
                    (\f -> { value | wrongAnswers = f })
                    (Frontend.EditorTheme.listEditor
                        "String"
                        Frontend.EditorTheme.stringEditor
                        ""
                        (level + 1)
                        value.wrongAnswers
                    )
              )
            ]
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


a11yOptionsDefault : Model.A11yOptions
a11yOptionsDefault =
    { unlockEverything = True
    , openDyslexic = True
    , fontSize = 0
    , opaqueBackgrounds = True
    }


sharedGameModelDefault : Model.SharedGameModel
sharedGameModelDefault =
    { currentPerson = idDefault, tickets = Set.empty, usedTickets = Set.empty }


gameModelDefault : Model.GameModel
gameModelDefault =
    Model.ViewingPerson


mapModelDefault : Model.MapModel
mapModelDefault =
    { travellingTo = Maybe.Nothing }


talkingModelDefault : Model.TalkingModel
talkingModelDefault =
    { chatHistory = chatHistoryDefault, currentDialog = dialogDefault }


chatHistoryDefault : Model.ChatHistory
chatHistoryDefault =
    []


chatLineDefault : Model.ChatLine
chatLineDefault =
    { image = "", name = "", line = "" }


menuModelDefault : Model.MenuModel
menuModelDefault =
    { previous = gameModelDefault, background = "" }


dataDefault : Model.Data
dataDefault =
    Dict.empty


idDefault : Model.Id
idDefault =
    ""


personDefault : Model.Person
personDefault =
    { name = ""
    , city = cityDefault
    , image = ""
    , dialog = dialogDefault
    , quizzes = []
    }


cityDefault : Model.City
cityDefault =
    { name = cityNameDefault
    , text = ""
    , image = ""
    , coordinates = coordinatesDefault
    , nation = nationDefault
    , sound = soundDefault
    }


soundDefault : Model.Sound
soundDefault =
    { name = "", duration = 0 }


cityNameDefault : Model.CityName
cityNameDefault =
    ""


coordinatesDefault : Model.Coordinates
coordinatesDefault =
    { x = 0, y = 0 }


nationDefault : Model.Nation
nationDefault =
    Model.Austria


dialogDefault : Model.Dialog
dialogDefault =
    { text = "", choices = ( choiceDefault, [] ) }


choiceDefault : Model.Choice
choiceDefault =
    { text = "", next = nextDefault }


nextDefault : Model.Next
nextDefault =
    Model.NextViewMap


quizDefault : Model.Quiz
quizDefault =
    { question = ""
    , correctAnswer = ""
    , messageIfCorrect = ""
    , messageIfWrong = ""
    , wrongAnswers = []
    }
