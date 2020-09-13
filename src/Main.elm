module Main exposing (main)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (Html, button, div, td, text, tr)
import Html.Attributes
import Html.Events exposing (onClick)
import List exposing (filter, foldr, indexedMap, map)
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


type alias Model =
    { fileOk : Bool
    , questions : List Question
    , deletedQuestions : List Question
    , newQuestion : String
    }


type alias Question =
    String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { questions = [], deletedQuestions = [], newQuestion = "", fileOk = True }, Cmd.none )



-- UPDATE


type Msg
    = Download
    | Upload
    | Delete Int
    | Add String
    | Change String
    | SQLLoaded File
    | StringFromSQLLoaded String


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

        Upload ->
            ( model, requestSQL )

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

        SQLLoaded file ->
            ( model, read file )

        StringFromSQLLoaded sql ->
            case splitList (String.lines sql) of
                Ok ( toDelete, toInsert ) ->
                    ( { model | fileOk = True, questions = map getQuestion toInsert, deletedQuestions = map getQuestion toDelete }, Cmd.none )

                Err _ ->
                    ( { model | fileOk = False }, Cmd.none )


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
    let
        content =
            [ button [ onClick Download ] [ text "Download" ]
            , button [ onClick Upload ] [ text "Upload" ]
            , Html.table [] (map tableEntryFromQuestion (indexedMap Tuple.pair model.questions) ++ [ newQuestion model ])
            ]
    in
    if model.fileOk == True then
        div [] (Html.h1 [] [ text "Fragenrandomizer" ] :: content)

    else
        div [] (Html.h1 [] [ text "Fragenrandomizer" ] :: text "Die Datei hat leider nicht das richtige Format." :: content)


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



-- FILEUPLOAD


requestSQL : Cmd Msg
requestSQL =
    Select.file [ "text/x-sql" ] SQLLoaded


read : File -> Cmd Msg
read file =
    Task.perform StringFromSQLLoaded (File.toString file)


splitList : List String -> Result String ( List String, List String )
splitList lines =
    let
        listentry =
            List.head (List.filter (\tuple -> String.contains "-- Insert" (Tuple.second tuple)) (List.indexedMap Tuple.pair lines))
    in
    case listentry of
        Just tuple ->
            Ok ( List.take (Tuple.first tuple) lines, List.filter (String.contains "DELETE") (List.drop (Tuple.first tuple + 1) lines) )

        Nothing ->
            Err "Falsches Format der Datei"


getQuestion : String -> String
getQuestion sql =
    String.dropRight 3 (String.dropLeft 43 sql)



-- FILECREATION DOWNLOAD


type QuestionType
    = ToDelete
    | ToInsert


download : String -> Cmd msg
download sqlContent =
    Download.string "Fragen.sql" "text/plain" sqlContent


sqlFromModel : Model -> String
sqlFromModel model =
    sqlFromList model.deletedQuestions ToDelete ++ "-- Insert \n" ++ sqlFromList model.questions ToDelete ++ sqlFromList model.questions ToInsert


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
