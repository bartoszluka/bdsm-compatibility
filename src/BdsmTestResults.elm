module BdsmTestResults exposing (BdsmTestResults, MissingKink(..), MyError(..), average, calculateScore, calculateScore2, calculateScore2Squared, calculateScore3, calculateScore3Squared, calculateScoreSquared, parseInput, perfectMatch, showError)

import Dict exposing (Dict)
import Parser exposing ((|.), (|=), DeadEnd, Parser, int, spaces, succeed, symbol)
import ParserExtra exposing (deadEndToString)


type alias Percentage =
    Int


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


type MyError
    = ParsingError DeadEnd
    | MissingValue String


showError : MyError -> String
showError error =
    case error of
        ParsingError deadEnd ->
            deadEndToString deadEnd

        MissingValue value ->
            "it looks like you didn't provide field" ++ value


parseInput : String -> Result (List MyError) BdsmTestResults
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
        |> Result.mapError (List.map ParsingError)
        |> Result.andThen toRecord


type alias BdsmTestResults =
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


kinkPairsNoDoubles : List ( BdsmTestResults -> Percentage, BdsmTestResults -> Percentage )
kinkPairsNoDoubles =
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


kinkPairsToDouble : List ( BdsmTestResults -> Percentage, BdsmTestResults -> Percentage )
kinkPairsToDouble =
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


calculateScore2 : BdsmTestResults -> BdsmTestResults -> Float
calculateScore2 first second =
    let
        difference ( selector1, selector2 ) =
            abs (selector1 first - selector2 second)
                + abs (selector2 first - selector1 second)

        score list =
            case average list of
                Nothing ->
                    0

                Just value ->
                    (100 - (value / 2)) / 100
    in
    kinkPairsToDouble
        |> List.map (difference >> toFloat)
        |> score


average : List Float -> Maybe Float
average list =
    let
        sum =
            List.sum list

        len =
            List.length list |> toFloat
    in
    if len == 0 then
        Nothing

    else
        Just <| sum / len


calculateScore2Squared : BdsmTestResults -> BdsmTestResults -> Float
calculateScore2Squared first second =
    let
        difference ( selector1, selector2 ) =
            (selector1 first - selector2 second) ^ 2 + (selector2 first - selector1 second) ^ 2

        score list =
            case average list of
                Nothing ->
                    0

                Just value ->
                    ((100 ^ 2) - (value / 2)) / (100 ^ 2)
    in
    kinkPairsToDouble
        |> List.map (difference >> toFloat)
        |> score


calculateScoreSquared : BdsmTestResults -> BdsmTestResults -> Float
calculateScoreSquared first second =
    let
        difference ( selector1, selector2 ) =
            (selector1 first - selector2 second) ^ 2

        score list =
            case average list of
                Nothing ->
                    0

                Just value ->
                    ((100 ^ 2) - value) / (100 ^ 2)
    in
    kinkPairsNoDoubles
        |> List.map (difference >> toFloat)
        |> score


calculateScore : BdsmTestResults -> BdsmTestResults -> Float
calculateScore first second =
    let
        difference ( selector1, selector2 ) =
            abs (selector1 first - selector2 second)

        score list =
            case average list of
                Nothing ->
                    0

                Just value ->
                    (100 - value) / 100
    in
    kinkPairsNoDoubles
        |> List.map (difference >> toFloat)
        |> score


calculateScore3Squared : BdsmTestResults -> BdsmTestResults -> Float
calculateScore3Squared first second =
    let
        scale =
            5

        difference ( selector1, selector2 ) =
            let
                scaleDistance v =
                    (v - 50) * scale
            in
            (scaleDistance (selector1 first) - scaleDistance (selector2 second)) ^ 2

        score list =
            case average list of
                Nothing ->
                    0

                Just value ->
                    let
                        maxValue =
                            (scale * 100) ^ 2
                    in
                    (maxValue - value) / maxValue
    in
    kinkPairsNoDoubles
        |> List.map (difference >> toFloat)
        |> score


calculateScore3 : BdsmTestResults -> BdsmTestResults -> Float
calculateScore3 first second =
    let
        scale =
            5

        difference ( selector1, selector2 ) =
            let
                scaleDistance v =
                    (v - 50) * scale
            in
            abs (scaleDistance (selector1 first) - scaleDistance (selector2 second))

        score list =
            case average list of
                Nothing ->
                    0

                Just value ->
                    (scale * 100 - value) / (scale * 100)
    in
    kinkPairsNoDoubles
        |> List.map (difference >> toFloat)
        |> score


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


type MissingKink
    = MissingKink String


toRecord : Dict String Percentage -> Result (List MyError) BdsmTestResults
toRecord dict =
    let
        get key =
            case Dict.get key dict of
                Just value ->
                    Ok value

                Nothing ->
                    Err key

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

        addToRecordOrNotFound : Result String a -> Result (List MyError) (a -> b) -> Result (List MyError) b
        addToRecordOrNotFound valueToInsert recordBuilder =
            case recordBuilder of
                Err errorList ->
                    case valueToInsert of
                        Err notFound ->
                            Err (MissingValue notFound :: errorList)

                        Ok _ ->
                            Err errorList

                Ok soFarOk ->
                    case valueToInsert of
                        Err notFound ->
                            Err [ MissingValue notFound ]

                        Ok found ->
                            Ok (soFarOk found)
    in
    Ok BdsmTestResults
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


perfectMatch : BdsmTestResults -> BdsmTestResults
perfectMatch { ageplayer, boyOrGirl, brat, bratTamer, daddyOrMommy, degradee, degrader, dominant, exhibitionist, experimentalist, masochist, masterOrMistress, nonMonogamist, owner, pet, primalHunter, primalPrey, rigger, ropeBunny, sadist, slave, submissive, switch, vanilla, voyeur } =
    { submissive = dominant
    , dominant = submissive
    , rigger = ropeBunny
    , ropeBunny = rigger
    , switch = switch
    , sadist = masochist
    , masochist = sadist
    , brat = bratTamer
    , bratTamer = brat
    , experimentalist = experimentalist
    , nonMonogamist = nonMonogamist
    , primalHunter = primalPrey
    , primalPrey = primalHunter
    , vanilla = vanilla
    , masterOrMistress = slave
    , slave = masterOrMistress
    , degrader = degradee
    , degradee = degrader
    , owner = pet
    , pet = owner
    , exhibitionist = voyeur
    , voyeur = exhibitionist
    , boyOrGirl = daddyOrMommy
    , daddyOrMommy = boyOrGirl
    , ageplayer = ageplayer
    }
