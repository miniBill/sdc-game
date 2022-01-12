module Editors exposing (personEditor, personDefault)

{-|

@docs personEditor, personDefault

-}

import Frontend.EditorTheme
import Model


personEditor : Model.Person -> Int -> Frontend.EditorTheme.Element Model.Person
personEditor value level =
    let
        rawSimples =
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

        rawComplexes =
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
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


cityEditor : Model.City -> Int -> Frontend.EditorTheme.Element Model.City
cityEditor value level =
    let
        rawSimples =
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

        rawComplexes =
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
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


soundEditor : Model.Sound -> Int -> Frontend.EditorTheme.Element Model.Sound
soundEditor value level =
    let
        rawSimples =
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

        rawComplexes =
            []
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


cityNameEditor : Model.CityName -> Int -> Frontend.EditorTheme.Element Model.CityName
cityNameEditor value level =
    Frontend.EditorTheme.stringEditor value level


coordinatesEditor : Model.Coordinates -> Int -> Frontend.EditorTheme.Element Model.Coordinates
coordinatesEditor value level =
    let
        rawSimples =
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

        rawComplexes =
            []
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


nationEditor : Model.Nation -> Int -> Frontend.EditorTheme.Element Model.Nation
nationEditor value level =
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


dialogEditor : Model.Dialog -> Int -> Frontend.EditorTheme.Element Model.Dialog
dialogEditor value level =
    let
        rawSimples =
            [ ( "Text"
              , Frontend.EditorTheme.map
                    (\f -> { value | text = f })
                    (Frontend.EditorTheme.stringEditor value.text)
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
                        value.choices
                    )
              )
            ]
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


choiceEditor : Model.Choice -> Int -> Frontend.EditorTheme.Element Model.Choice
choiceEditor value level =
    let
        rawSimples =
            [ ( "Text"
              , Frontend.EditorTheme.map
                    (\f -> { value | text = f })
                    (Frontend.EditorTheme.stringEditor value.text)
              )
            ]

        rawComplexes =
            [ ( "Next"
              , Frontend.EditorTheme.map
                    (\f -> { value | next = f })
                    (nextEditor value.next)
              )
            ]
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


nextEditor : Model.Next -> Int -> Frontend.EditorTheme.Element Model.Next
nextEditor value level =
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
    in
    Frontend.EditorTheme.customEditor variants inputsRow value level


quizEditor : Model.Quiz -> Int -> Frontend.EditorTheme.Element Model.Quiz
quizEditor value level =
    let
        rawSimples =
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

        rawComplexes =
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
    in
    Frontend.EditorTheme.objectEditor rawSimples rawComplexes level


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
