module Main exposing (main)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import File.Download as Download
import Html exposing (Html, button, div, td, text, tr)
import Html.Attributes
import Html.Events exposing (onClick)
import List exposing (filter, foldr, indexedMap, map)



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


type alias Model =
    { questions : List Question
    , deletedQuestions : List Question
    , newQuestion : String
    }


type alias Question =
    String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { questions = [], deletedQuestions = [], newQuestion = "" }, Cmd.none )



-- UPDATE


type Msg
    = Download
    | Delete Int
    | Add String
    | Change String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Delete index ->
            ( { model
                | questions = deleteElementFromQuestionList index model.questions
                , deletedQuestions = model.deletedQuestions ++ getElementFromQuestionList index model.questions
              }
            , Cmd.none
            )

        Download ->
            ( model, download (sqlFromModel model) )

        Add question ->
            ( { model
                | questions = model.questions ++ [ question ]
                , newQuestion = ""
              }
            , Cmd.none
            )

        Change changedQuestion ->
            ( { model
                | newQuestion = changedQuestion
              }
            , Cmd.none
            )


deleteElementFromQuestionList : Int -> List Question -> List Question
deleteElementFromQuestionList index questionlist =
    map Tuple.second (filter (\tuple -> (/=) index (Tuple.first tuple)) (indexedMap Tuple.pair questionlist))


getElementFromQuestionList : Int -> List Question -> List Question
getElementFromQuestionList index questionlist =
    map Tuple.second (filter (\tuple -> (==) index (Tuple.first tuple)) (indexedMap Tuple.pair questionlist))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Download ] [ text "Download" ]
        , Html.table [] (map tableEntryFromQuestion (indexedMap Tuple.pair model.questions) ++ [ newQuestion model ])
        ]


tableEntryFromQuestion : ( Int, Question ) -> Html Msg
tableEntryFromQuestion ( number, question ) =
    tr []
        [ td [] [ text question ]
        , button [ onClick (Delete number) ] [ text "Löschen" ]
        ]


newQuestion : Model -> Html Msg
newQuestion model =
    tr []
        [ td [] [ Html.input [ Html.Attributes.placeholder "neue Frage", Html.Attributes.value model.newQuestion, Html.Events.onInput Change ] [] ]
        , button [ onClick (Add model.newQuestion) ] [ text "Hinzufügen" ]
        ]



-- FILECREATION


type QuestionType
    = ToDelete
    | ToInsert


download : String -> Cmd msg
download sqlContent =
    Download.string "Fragen.sql" "text/plain" sqlContent


sqlFromModel : Model -> String
sqlFromModel model =
    sqlFromList model.questions ToDelete ++ sqlFromList model.deletedQuestions ToDelete ++ sqlFromList model.questions ToInsert


sqlFromList : List Question -> QuestionType -> String
sqlFromList questionlist questionType =
    foldr (++) "" (map (\question -> sqlFromQuestion questionType question) questionlist)


sqlFromQuestion : QuestionType -> Question -> String
sqlFromQuestion questionType question =
    case questionType of
        ToDelete ->
            "DELETE FROM questions WHERE questiontext= '" ++ question ++ "'; \n"

        ToInsert ->
            "INSERT INTO questions SET questiontext='" ++ question ++ "'; \n"
