module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Parser exposing ((|.), (|=), DeadEnd, Parser, int, spaces, succeed, symbol)
import ParserExtra exposing (deadEndToString)


lineParser : Parser ( String, Percentage )
lineParser =
    let
        kinkName : Parser String
        kinkName =
            Parser.chompUntilEndOr "\n"
                |> Parser.getChompedString
                |> Parser.map String.trimRight
    in
    succeed (\percent name -> ( name, percent ))
        |. spaces
        |= int
        |. symbol "%"
        |. spaces
        |= kinkName


type alias Percentage =
    Int


type alias TestResults =
    { ageplayer : Percentage
    , boyOrGirl : Percentage
    , brat : Percentage
    , bratTamer : Percentage
    , daddyOrMommy : Percentage
    , degradee : Percentage
    , degrader : Percentage
    , dominant : Percentage
    , exhibitionist : Percentage
    , experimentalist : Percentage
    , masochist : Percentage
    , masterOrMistress : Percentage
    , nonMonogamist : Percentage
    , owner : Percentage
    , pet : Percentage
    , primalHunter : Percentage
    , primalPrey : Percentage
    , rigger : Percentage
    , ropeBunny : Percentage
    , sadist : Percentage
    , slave : Percentage
    , submissive : Percentage
    , switch : Percentage
    , vanilla : Percentage
    , voyeur : Percentage
    }


calculateScore2 : TestResults -> TestResults -> Float
calculateScore2 first second =
    let
        difference ( selector1, selector2 ) =
            abs (selector1 first - selector2 second)
                + abs (selector2 first - selector1 second)

        score list =
            let
                sum =
                    List.sum list

                len =
                    List.length list
                        |> toFloat
            in
            if len == 0 then
                0

            else
                100 - (sum / (len * 2))
    in
    [ ( .submissive, .dominant )
    , ( .rigger, .ropeBunny )
    , ( .switch, .switch )
    , ( .sadist, .masochist )
    , ( .brat, .bratTamer )
    , ( .experimentalist, .experimentalist )
    , ( .nonMonogamist, .nonMonogamist )
    , ( .primalHunter, .primalPrey )
    , ( .vanilla, .vanilla )
    , ( .masterOrMistress, .slave )
    , ( .degrader, .degradee )
    , ( .owner, .pet )
    , ( .exhibitionist, .voyeur )
    , ( .boyOrGirl, .daddyOrMommy )
    , ( .ageplayer, .ageplayer )
    ]
        |> List.map (difference >> toFloat)
        |> score


calculateScore : TestResults -> TestResults -> Float
calculateScore first second =
    let
        difference ( selector1, selector2 ) =
            abs (selector1 first - selector2 second)

        score list =
            let
                sum =
                    List.sum list

                len =
                    List.length list
                        |> toFloat
            in
            if len == 0 then
                0

            else
                100 - (sum / len)
    in
    [ ( .submissive, .dominant )
    , ( .dominant, .submissive )
    , ( .rigger, .ropeBunny )
    , ( .ropeBunny, .rigger )
    , ( .switch, .switch )
    , ( .sadist, .masochist )
    , ( .masochist, .sadist )
    , ( .brat, .bratTamer )
    , ( .bratTamer, .brat )
    , ( .experimentalist, .experimentalist )
    , ( .nonMonogamist, .nonMonogamist )
    , ( .primalHunter, .primalPrey )
    , ( .primalPrey, .primalHunter )
    , ( .vanilla, .vanilla )
    , ( .masterOrMistress, .slave )
    , ( .slave, .masterOrMistress )
    , ( .degrader, .degradee )
    , ( .degradee, .degrader )
    , ( .owner, .pet )
    , ( .pet, .owner )
    , ( .exhibitionist, .voyeur )
    , ( .voyeur, .exhibitionist )
    , ( .boyOrGirl, .daddyOrMommy )
    , ( .daddyOrMommy, .boyOrGirl )
    , ( .ageplayer, .ageplayer )
    ]
        |> List.map (difference >> toFloat)
        |> score


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Error =
    String


type alias Model =
    { firstInput : String
    , secondInput : String
    , firstPersonResults : Result (List Error) TestResults
    , secondPersonResults : Result (List Error) TestResults
    , finalScore : Maybe Float
    }


init : Model
init =
    { firstInput = ""
    , secondInput = ""
    , firstPersonResults = Err []
    , secondPersonResults = Err []
    , finalScore = Nothing
    }



-- UPDATE


type Msg
    = ParseFirst String
    | ParseSecond String


update : Msg -> Model -> Model
update msg model =
    let
        parseInput input =
            let
                isJust maybe =
                    case maybe of
                        Just _ ->
                            True

                        Nothing ->
                            False

                startsWithDigit =
                    String.trimLeft >> String.left 1 >> String.toInt >> isJust
            in
            input
                |> String.lines
                -- Maybe parse the lines here so that this filtering is unnecessary
                |> List.filter startsWithDigit
                |> List.map (Parser.run lineParser)
                |> toDict
                |> Result.mapError (List.map deadEndToString)
                |> Result.andThen toRecord
    in
    case msg of
        ParseFirst newContent ->
            { model
                | firstInput = newContent
                , firstPersonResults =
                    parseInput newContent
            }

        ParseSecond newContent ->
            { model
                | secondInput = newContent
                , secondPersonResults =
                    parseInput newContent
            }



-- VIEW


toDict : List (Result (List DeadEnd) ( String, Percentage )) -> Result (List DeadEnd) (Dict String Percentage)
toDict results =
    let
        toAssocs : List (Result (List DeadEnd) ( String, Percentage )) -> Result (List DeadEnd) (List ( String, Percentage ))
        toAssocs =
            List.foldl
                (Result.map2 (::))
                (Ok [])
    in
    results |> toAssocs |> Result.map Dict.fromList


myDictGet : comparable -> Dict comparable v -> Result comparable v
myDictGet key dict =
    case Dict.get key dict of
        Just value ->
            Ok value

        Nothing ->
            Err key


toRecord : Dict String Percentage -> Result (List String) TestResults
toRecord dict =
    let
        get key =
            myDictGet key dict

        ageplayer =
            get "Ageplayer"

        boyOrGirl =
            get "Boy/Girl"

        brat =
            get "Brat"

        bratTamer =
            get "Brat tamer"

        daddyOrMommy =
            get "Daddy/Mommy"

        degradee =
            get "Degradee"

        degrader =
            get "Degrader"

        dominant =
            get "Dominant"

        exhibitionist =
            get "Exhibitionist"

        experimentalist =
            get "Experimentalist"

        masochist =
            get "Masochist"

        masterOrMistress =
            get "Master/Mistress"

        nonMonogamist =
            get "Non-monogamist"

        owner =
            get "Owner"

        pet =
            get "Pet"

        primalHunter =
            get "Primal (Hunter)"

        primalPrey =
            get "Primal (Prey)"

        rigger =
            get "Rigger"

        ropeBunny =
            get "Rope bunny"

        sadist =
            get "Sadist"

        slave =
            get "Slave"

        submissive =
            get "Submissive"

        switch =
            get "Switch"

        vanilla =
            get "Vanilla"

        voyeur =
            get "Voyeur"

        addToRecordOrNotFound : Result String a -> Result (List String) (a -> b) -> Result (List String) b
        addToRecordOrNotFound valueToInsert recordBuilder =
            case recordBuilder of
                Err errorList ->
                    case valueToInsert of
                        Err notFound ->
                            Err (notFound :: errorList)

                        Ok _ ->
                            Err errorList

                Ok soFarOk ->
                    case valueToInsert of
                        Err notFound ->
                            Err [ notFound ]

                        Ok found ->
                            Ok (soFarOk found)
    in
    Ok TestResults
        |> addToRecordOrNotFound ageplayer
        |> addToRecordOrNotFound boyOrGirl
        |> addToRecordOrNotFound brat
        |> addToRecordOrNotFound bratTamer
        |> addToRecordOrNotFound daddyOrMommy
        |> addToRecordOrNotFound degradee
        |> addToRecordOrNotFound degrader
        |> addToRecordOrNotFound dominant
        |> addToRecordOrNotFound exhibitionist
        |> addToRecordOrNotFound experimentalist
        |> addToRecordOrNotFound masochist
        |> addToRecordOrNotFound masterOrMistress
        |> addToRecordOrNotFound nonMonogamist
        |> addToRecordOrNotFound owner
        |> addToRecordOrNotFound pet
        |> addToRecordOrNotFound primalHunter
        |> addToRecordOrNotFound primalPrey
        |> addToRecordOrNotFound rigger
        |> addToRecordOrNotFound ropeBunny
        |> addToRecordOrNotFound sadist
        |> addToRecordOrNotFound slave
        |> addToRecordOrNotFound submissive
        |> addToRecordOrNotFound switch
        |> addToRecordOrNotFound vanilla
        |> addToRecordOrNotFound voyeur


view : Model -> Html Msg
view model =
    div []
        [ textarea [ placeholder "first input", value model.firstInput, onInput ParseFirst ] []
        , textarea [ placeholder "second input", value model.secondInput, onInput ParseSecond ] []
        , div []
            [ model.firstPersonResults
                |> Debug.toString
                |> text
            ]
        , div []
            [ model.secondPersonResults
                |> Debug.toString
                |> text
            ]
        , div []
            [ model.finalScore
                |> Maybe.map String.fromFloat
                |> Maybe.withDefault "not calculated"
                |> text
            ]
        ]
