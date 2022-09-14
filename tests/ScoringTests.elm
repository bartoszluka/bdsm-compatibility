module ScoringTests exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Main exposing (..)
import Test exposing (..)


exampleResult1 : TestResults
exampleResult1 =
    { dominant = 98
    , rigger = 94
    , sadist = 94
    , bratTamer = 94
    , experimentalist = 76
    , nonMonogamist = 61
    , masochist = 58
    , primalHunter = 56
    , vanilla = 46
    , masterOrMistress = 33
    , degrader = 32
    , owner = 27
    , switch = 19
    , ropeBunny = 11
    , exhibitionist = 7
    , submissive = 4
    , voyeur = 3
    , boyOrGirl = 0
    , slave = 0
    , daddyOrMommy = 0
    , primalPrey = 0
    , pet = 0
    , brat = 0
    , degradee = 0
    , ageplayer = 0
    }


exampleResult2 : TestResults
exampleResult2 =
    { submissive = 98
    , masochist = 78
    , ropeBunny = 78
    , experimentalist = 75
    , degradee = 75
    , vanilla = 68
    , voyeur = 48
    , nonMonogamist = 28
    , sadist = 24
    , switch = 19
    , primalPrey = 13
    , slave = 9
    , rigger = 8
    , dominant = 6
    , exhibitionist = 0
    , ageplayer = 0
    , pet = 0
    , brat = 0
    , owner = 0
    , masterOrMistress = 0
    , boyOrGirl = 0
    , degrader = 0
    , daddyOrMommy = 0
    , bratTamer = 0
    , primalHunter = 0
    }


testResultsFuzzer : Fuzzer TestResults
testResultsFuzzer =
    let
        percent =
            Fuzz.intRange 0 100

        apply x f =
            f x
    in
    Fuzz.constant TestResults
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent
        |> Fuzz.map2 apply percent


suite : Test
suite =
    describe "The TestResults module"
        [ fuzz testResultsFuzzer "every score should be between 0 and 100" <|
            \testResults ->
                let
                    score =
                        calculateScore testResults testResults
                in
                Expect.all
                    [ Expect.atLeast 0
                    , Expect.atMost 100
                    ]
                    score
        ]
