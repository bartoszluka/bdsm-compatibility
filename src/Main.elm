module Main exposing (..)

-- A text input for reversing text. Very useful!
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/text_fields.html

import Browser
import Html exposing (Html, div, input, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Parser exposing ((|.), (|=), DeadEnd, Parser, float, int, keyword, spaces, succeed, symbol)
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


lineParser : Parser KinkPercent
lineParser =
    let
        kinkName : Parser Kink
        kinkName =
            Parser.getChompedString
                (Parser.chompUntilEndOr " ")
                |> Parser.andThen kinkParser
    in
    succeed (\percent name -> KinkPercent name percent)
        |. spaces
        |= int
        |. symbol "%"
        |. spaces
        |= kinkName


type alias KinkPercent =
    { kink : Kink
    , percent : Int
    }


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
    , results : Result (List DeadEnd) KinkPercent
    }


init : Model
init =
    { content = "", results = Err [] }



-- UPDATE


type Msg
    = Parse String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Parse newContent ->
            { model
                | content = newContent
                , results = Parser.run lineParser newContent
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


viewKinkPercent : KinkPercent -> String
viewKinkPercent kinkPercent =
    showKink kinkPercent.kink ++ "\t" ++ String.fromInt kinkPercent.percent ++ "%"



-- VIEW


view : Model -> Html Msg
view model =
    let
        results =
            case model.results of
                Err errors ->
                    deadEndsToString errors

                Ok kinkPercent ->
                    viewKinkPercent kinkPercent
    in
    div []
        [ input [ placeholder "Kink percent", value model.content, onInput Parse ] []
        , div [] [ text results ]
        ]
