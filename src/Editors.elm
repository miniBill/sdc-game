module Editors exposing (personEditor, personDefault)

{-|

@docs personEditor, personDefault

-}

import Frontend.EditorTheme
import Model


personEditor : Model.Person -> Frontend.EditorTheme.Editor Model.Person
personEditor value =
    Frontend.EditorTheme.objectEditor
        [ ( "Name"
          , Frontend.EditorTheme.map
                (\f -> { value | name = f })
                (Frontend.EditorTheme.stringEditor value.name)
          )
        , ( "Image"
          , Frontend.EditorTheme.map
                (\f -> { value | image = f })
                (Frontend.EditorTheme.stringEditor value.image)
          )
        ]
        [ ( "City"
          , Frontend.EditorTheme.map
                (\f -> { value | city = f })
                (cityEditor value.city)
          )
        , ( "Dialog"
          , Frontend.EditorTheme.map
                (\f -> { value | dialog = f })
                (dialogEditor value.dialog)
          )
        , ( "Quizzes"
          , Frontend.EditorTheme.map
                (\f -> { value | quizzes = f })
                (Frontend.EditorTheme.listEditor
                    "Quiz"
                    quizEditor
                    quizDefault
                    value.quizzes
                )
          )
        ]


cityEditor : Model.City -> Frontend.EditorTheme.Editor Model.City
cityEditor value =
    Frontend.EditorTheme.objectEditor
        [ ( "Name"
          , Frontend.EditorTheme.map
                (\f -> { value | name = f })
                (cityNameEditor value.name)
          )
        , ( "Text"
          , Frontend.EditorTheme.map
                (\f -> { value | text = f })
                (Frontend.EditorTheme.stringEditor value.text)
          )
        , ( "Image"
          , Frontend.EditorTheme.map
                (\f -> { value | image = f })
                (Frontend.EditorTheme.stringEditor value.image)
          )
        ]
        [ ( "Coordinates"
          , Frontend.EditorTheme.map
                (\f -> { value | coordinates = f })
                (coordinatesEditor value.coordinates)
          )
        , ( "Nation"
          , Frontend.EditorTheme.map
                (\f -> { value | nation = f })
                (nationEditor value.nation)
          )
        , ( "Sound"
          , Frontend.EditorTheme.map
                (\f -> { value | sound = f })
                (soundEditor value.sound)
          )
        ]


soundEditor : Model.Sound -> Frontend.EditorTheme.Editor Model.Sound
soundEditor value =
    Frontend.EditorTheme.objectEditor
        [ ( "Name"
          , Frontend.EditorTheme.map
                (\f -> { value | name = f })
                (Frontend.EditorTheme.stringEditor value.name)
          )
        , ( "Duration"
          , Frontend.EditorTheme.map
                (\f -> { value | duration = f })
                (Frontend.EditorTheme.intEditor value.duration)
          )
        ]
        []


cityNameEditor : Model.CityName -> Frontend.EditorTheme.Editor Model.CityName
cityNameEditor value =
    Frontend.EditorTheme.stringEditor value


coordinatesEditor : Model.Coordinates -> Frontend.EditorTheme.Editor Model.Coordinates
coordinatesEditor value =
    Frontend.EditorTheme.objectEditor
        [ ( "X"
          , Frontend.EditorTheme.map
                (\f -> { value | x = f })
                (Frontend.EditorTheme.floatEditor value.x)
          )
        , ( "Y"
          , Frontend.EditorTheme.map
                (\f -> { value | y = f })
                (Frontend.EditorTheme.floatEditor value.y)
          )
        ]
        []


nationEditor : Model.Nation -> Frontend.EditorTheme.Editor Model.Nation
nationEditor value =
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
    Frontend.EditorTheme.enumEditor variants value


dialogEditor : Model.Dialog -> Frontend.EditorTheme.Editor Model.Dialog
dialogEditor value =
    Frontend.EditorTheme.objectEditor
        [ ( "Text"
          , Frontend.EditorTheme.map
                (\f -> { value | text = f })
                (Frontend.EditorTheme.stringEditor value.text)
          )
        ]
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
                    value.choices
                )
          )
        ]


choiceEditor : Model.Choice -> Frontend.EditorTheme.Editor Model.Choice
choiceEditor value =
    Frontend.EditorTheme.objectEditor
        [ ( "Text"
          , Frontend.EditorTheme.map
                (\f -> { value | text = f })
                (Frontend.EditorTheme.stringEditor value.text)
          )
        ]
        [ ( "Next"
          , Frontend.EditorTheme.map
                (\f -> { value | next = f })
                (nextEditor value.next)
          )
        ]


nextEditor : Model.Next -> Frontend.EditorTheme.Editor Model.Next
nextEditor value =
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

                Model.NextWin ->
                    extractedDefault

        variants =
            [ ( "Dialog", Model.NextDialog dialogExtracted )
            , ( "View map", Model.NextViewMap )
            , ( "Random quiz", Model.NextRandomQuiz )
            , ( "Quiz", Model.NextQuiz quizExtracted )
            , ( "Give ticket", Model.NextGiveTicket )
            , ( "Win", Model.NextWin )
            ]

        inputsRow =
            case value of
                Model.NextDialog dialog ->
                    [ Frontend.EditorTheme.map
                        (\f -> Model.NextDialog f)
                        (dialogEditor dialog)
                    ]

                Model.NextViewMap ->
                    []

                Model.NextRandomQuiz ->
                    []

                Model.NextQuiz quiz ->
                    [ Frontend.EditorTheme.map
                        (\f -> Model.NextQuiz f)
                        (quizEditor quiz)
                    ]

                Model.NextGiveTicket ->
                    []

                Model.NextWin ->
                    []
    in
    Frontend.EditorTheme.customEditor variants inputsRow value


quizEditor : Model.Quiz -> Frontend.EditorTheme.Editor Model.Quiz
quizEditor value =
    Frontend.EditorTheme.objectEditor
        [ ( "Question"
          , Frontend.EditorTheme.map
                (\f -> { value | question = f })
                (Frontend.EditorTheme.stringEditor value.question)
          )
        , ( "Correct answer"
          , Frontend.EditorTheme.map
                (\f -> { value | correctAnswer = f })
                (Frontend.EditorTheme.stringEditor value.correctAnswer)
          )
        , ( "Message if correct"
          , Frontend.EditorTheme.map
                (\f -> { value | messageIfCorrect = f })
                (Frontend.EditorTheme.stringEditor value.messageIfCorrect)
          )
        , ( "Message if wrong"
          , Frontend.EditorTheme.map
                (\f -> { value | messageIfWrong = f })
                (Frontend.EditorTheme.stringEditor value.messageIfWrong)
          )
        ]
        [ ( "Wrong answers"
          , Frontend.EditorTheme.map
                (\f -> { value | wrongAnswers = f })
                (Frontend.EditorTheme.listEditor
                    "String"
                    Frontend.EditorTheme.stringEditor
                    ""
                    value.wrongAnswers
                )
          )
        ]


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
