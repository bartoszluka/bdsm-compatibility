module Main exposing (..)

-- A text input for reversing text. Very useful!
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/text_fields.html

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, input, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Parser exposing ((|.), (|=), DeadEnd, Parser, int, spaces, succeed, symbol)
import ParserExtra exposing (deadEndsToString)



-- MAIN
-- == Results from bdsmtest.org ==
-- 98% Dominant
-- 94% Rigger
-- 94% Sadist
-- 94% Brat tamer
-- 76% Experimentalist
-- 61% Non-monogamist
-- 58% Masochist
-- 56% Primal (Hunter)
-- 46% Vanilla
-- 33% Master/Mistress
-- 32% Degrader
-- 27% Owner
-- 19% Switch
-- 11% Rope bunny
-- 7% Exhibitionist
-- 4% Submissive
-- 3% Voyeur
-- 0% Boy/Girl
-- 0% Slave
-- 0% Daddy/Mommy
-- 0% Primal (Prey)
-- 0% Pet
-- 0% Brat
-- 0% Degradee
-- 0% Ageplayer


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


kinkParser : String -> Parser Kink
kinkParser input =
    case input of
        "Dominant" ->
            succeed Dominant

        "Rigger" ->
            succeed Rigger

        "Sadist" ->
            succeed Sadist

        "Brat tamer" ->
            succeed BratTamer

        "Experimentalist" ->
            succeed Experimentalist

        "Non-monogamist" ->
            succeed NonMonogamist

        "Masochist" ->
            succeed Masochist

        "Primal (Hunter)" ->
            succeed PrimalHunter

        "Vanilla" ->
            succeed Vanilla

        "Master/Mistress" ->
            succeed MasterOrMistress

        "Degrader" ->
            succeed Degrader

        "Owner" ->
            succeed Owner

        "Switch" ->
            succeed Switch

        "Rope bunny" ->
            succeed RopeBunny

        "Exhibitionist" ->
            succeed Exhibitionist

        "Submissive" ->
            succeed Submissive

        "Voyeur" ->
            succeed Voyeur

        "Boy/Girl" ->
            succeed BoyOrGirl

        "Slave" ->
            succeed Slave

        "Daddy/Mommy" ->
            succeed DaddyOrMommy

        "Primal (Prey)" ->
            succeed PrimalPrey

        "Pet" ->
            succeed Pet

        "Brat" ->
            succeed Brat

        "Degradee" ->
            succeed Degradee

        "Ageplayer" ->
            succeed Ageplayer

        _ ->
            Parser.problem ("kink name " ++ input ++ " is not recognized")


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


type Kink
    = Dominant
    | Rigger
    | Sadist
    | BratTamer
    | Experimentalist
    | NonMonogamist
    | Masochist
    | PrimalHunter
    | Vanilla
    | MasterOrMistress
    | Degrader
    | Owner
    | Switch
    | RopeBunny
    | Exhibitionist
    | Submissive
    | Voyeur
    | BoyOrGirl
    | Slave
    | DaddyOrMommy
    | PrimalPrey
    | Pet
    | Brat
    | Degradee
    | Ageplayer


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { content : String
    , results : List (Result (List DeadEnd) ( String, Percentage ))
    , resultsDict : Dict String Percentage
    }


init : Model
init =
    { content = "", results = [], resultsDict = Dict.empty }



-- UPDATE


type Msg
    = Parse String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Parse newContent ->
            { model
                | content = newContent

                -- , results = Parser.run lineParser newContent
                , results = newContent |> String.lines |> List.map (Parser.run lineParser)
            }


showKink : Kink -> String
showKink kink =
    case kink of
        Dominant ->
            "Dominant"

        Rigger ->
            "Rigger"

        Sadist ->
            "Sadist"

        BratTamer ->
            "Brat tamer"

        Experimentalist ->
            "Experimentalist"

        NonMonogamist ->
            "Non-monogamist"

        Masochist ->
            "Masochist"

        PrimalHunter ->
            "Primal (Hunter)"

        Vanilla ->
            "Vanilla"

        MasterOrMistress ->
            "Master/Mistress"

        Degrader ->
            "Degrader"

        Owner ->
            "Owner"

        Switch ->
            "Switch"

        RopeBunny ->
            "Rope bunny"

        Exhibitionist ->
            "Exhibitionist"

        Submissive ->
            "Submissive"

        Voyeur ->
            "Voyeur"

        BoyOrGirl ->
            "Boy/Girl"

        Slave ->
            "Slave"

        DaddyOrMommy ->
            "Daddy/Mommy"

        PrimalPrey ->
            "Primal (Prey)"

        Pet ->
            "Pet"

        Brat ->
            "Brat"

        Degradee ->
            "Degradee"

        Ageplayer ->
            "Ageplayer"



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


toRecord : Dict String Percentage -> Result String TestResults
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

        apply x f =
            f x
    in
    Ok TestResults
        |> Result.map2 apply ageplayer
        |> Result.map2 apply boyOrGirl
        |> Result.map2 apply brat
        |> Result.map2 apply bratTamer
        |> Result.map2 apply daddyOrMommy
        |> Result.map2 apply degradee
        |> Result.map2 apply degrader
        |> Result.map2 apply dominant
        |> Result.map2 apply exhibitionist
        |> Result.map2 apply experimentalist
        |> Result.map2 apply masochist
        |> Result.map2 apply masterOrMistress
        |> Result.map2 apply nonMonogamist
        |> Result.map2 apply owner
        |> Result.map2 apply pet
        |> Result.map2 apply primalHunter
        |> Result.map2 apply primalPrey
        |> Result.map2 apply rigger
        |> Result.map2 apply ropeBunny
        |> Result.map2 apply sadist
        |> Result.map2 apply slave
        |> Result.map2 apply submissive
        |> Result.map2 apply switch
        |> Result.map2 apply vanilla
        |> Result.map2 apply voyeur


view : Model -> Html Msg
view model =
    let
        toString results =
            case results of
                Err errors ->
                    deadEndsToString errors

                Ok ( name, percentage ) ->
                    name ++ "\t" ++ String.fromInt percentage ++ "%"
    in
    div []
        [ textarea [ placeholder "Kink percent", value model.content, onInput Parse ] []

        -- , div []
        --     (model.results
        --         |> List.map toString
        --         |> List.map (\dupa -> div [] [ text dupa ])
        --     )
        , div [] [ model.results |> toDict |> Result.mapError deadEndsToString |> Result.andThen toRecord |> Debug.toString |> text ]
        ]
