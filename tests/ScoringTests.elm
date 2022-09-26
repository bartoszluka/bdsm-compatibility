module ScoringTests exposing (..)

import BdsmTestResults exposing (..)
import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer)
import Test exposing (..)


exampleResult1 : BdsmTestResults
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


exampleResult2 : BdsmTestResults
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


exampleResult3 : BdsmTestResults
exampleResult3 =
    { brat = 94
    , submissive = 92
    , ropeBunny = 84
    , voyeur = 68
    , vanilla = 66
    , masochist = 62
    , switch = 58
    , degradee = 56
    , primalPrey = 55
    , nonMonogamist = 51
    , experimentalist = 50
    , dominant = 43
    , degrader = 40
    , primalHunter = 26
    , sadist = 24
    , slave = 16
    , rigger = 14
    , owner = 12
    , masterOrMistress = 12
    , bratTamer = 9
    , exhibitionist = 5
    , daddyOrMommy = 3
    , pet = 3
    , boyOrGirl = 0
    , ageplayer = 0
    }


testResultsFuzzer : Fuzzer BdsmTestResults
testResultsFuzzer =
    let
        percent =
            Fuzz.intRange 0 100

        apply x f =
            f x
    in
    Fuzz.constant BdsmTestResults
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
        [ skip <|
            fuzz (Fuzz.tuple ( testResultsFuzzer, testResultsFuzzer )) "every score should be between 0 and 1" <|
                \( results1, results2 ) ->
                    let
                        metrics =
                            [ calculateScore
                            , calculateScoreSquared
                            , calculateScore2
                            , calculateScore2Squared
                            , calculateScore3
                            ]

                        metricsMap metric ( res1, res2 ) =
                            Expect.all [ Expect.atLeast 0, Expect.atMost 1 ]
                                (metric res1 res2)
                    in
                    Expect.all
                        (List.map
                            metricsMap
                            metrics
                        )
                        ( results1, results2 )
        , test "example results" <|
            \_ ->
                let
                    score =
                        Debug.log "score no doubles\t\t" <| (*) 100 <| calculateScore exampleResult1 exampleResult3

                    scoreSquared =
                        Debug.log "score with doubles\t\t" <| (*) 100 <| calculateScoreSquared exampleResult1 exampleResult3

                    score2 =
                        Debug.log "score no doubles squared\t" <| (*) 100 <| calculateScore2 exampleResult1 exampleResult3

                    score2Squared =
                        Debug.log "score with doubles squared\t" <| (*) 100 <| calculateScore2Squared exampleResult1 exampleResult3

                    -- score3 =
                    --     Debug.log "score no doubles scaled\t\t" <| (*) 100 <| calculateScore3 exampleResult1 exampleResult3
                    -- score3Squared =
                    --     Debug.log "score no doubles scaled squared\t" <| (*) 100 <| calculateScore3Squared exampleResult1 exampleResult3
                in
                Expect.all
                    [ Expect.atLeast 0
                    , Expect.atMost 100
                    ]
                    score
        , skip <|
            test "same results" <|
                \_ ->
                    let
                        score =
                            Debug.log "score no doubles\t\t" <| (*) 100 <| calculateScore exampleResult1 exampleResult1

                        scoreSquared =
                            Debug.log "score with doubles\t\t" <| (*) 100 <| calculateScoreSquared exampleResult1 exampleResult1

                        score2 =
                            Debug.log "score no doubles squared\t" <| (*) 100 <| calculateScore2 exampleResult1 exampleResult1

                        score2Squared =
                            Debug.log "score with doubles squared\t" <| (*) 100 <| calculateScore2Squared exampleResult1 exampleResult1

                        -- score3 =
                        --     Debug.log "score no doubles scaled\t\t" <| (*) 100 <| calculateScore3 exampleResult1 exampleResult1
                        -- score3Squared =
                        --     Debug.log "score no doubles scaled squared\t" <| (*) 100 <| calculateScore3Squared exampleResult1 exampleResult1
                    in
                    Expect.all
                        [ Expect.atLeast 0
                        , Expect.atMost 100
                        ]
                        score
        , skip <|
            fuzz (Fuzz.tuple ( testResultsFuzzer, testResultsFuzzer )) "scores should be symetric" <|
                \( results1, results2 ) ->
                    let
                        calculation =
                            calculateScoreSquared

                        score1 =
                            calculation results1 results2

                        score2 =
                            calculation results2 results1
                    in
                    Expect.within (Expect.Absolute 1.0e-5) score1 score2
        , skip <|
            fuzz testResultsFuzzer "perfectMatch should give maximum score" <|
                \results ->
                    let
                        metrics =
                            [ calculateScore, calculateScoreSquared, calculateScore2, calculateScore2Squared ]

                        perfectMatchShouldBeMaximum metric result =
                            Expect.within (Expect.Absolute 0.00001) 1 (metric result (perfectMatch result))
                    in
                    Expect.all
                        (List.map
                            perfectMatchShouldBeMaximum
                            metrics
                        )
                        results
        , test "parsing is correct" <|
            \_ ->
                String.join "\n"
                    [ "== Results from bdsmtest.org == "
                    , "98% Dominant "
                    , "94% Rigger "
                    , "94% Sadist "
                    , "94% Brat tamer "
                    , "76% Experimentalist "
                    , "61% Non-monogamist "
                    , "58% Masochist "
                    , "56% Primal (Hunter) "
                    , "46% Vanilla "
                    , "33% Master/Mistress "
                    , "32% Degrader "
                    , "27% Owner "
                    , "19% Switch "
                    , "11% Rope bunny "
                    , "7% Exhibitionist "
                    , "4% Submissive "
                    , "3% Voyeur "
                    , "0% Boy/Girl "
                    , "0% Slave "
                    , "0% Daddy/Mommy "
                    , "0% Primal (Prey) "
                    , "0% Pet "
                    , "0% Brat "
                    , "0% Degradee "
                    , "0% Ageplayer "
                    ]
                    |> parseInput
                    |> Expect.equal (Ok exampleResult1)
        ]
