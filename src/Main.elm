module Main exposing (..)

import BdsmTestResults exposing (BdsmTestResults, MyError(..), calculateScore2, parseInput)
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button, labelAbove, multiline, placeholder)
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = viewLayout }


viewLayout : Model -> Html Msg
viewLayout model =
    layout [ Background.color backgroundColor, Font.color white ] (view model)



-- MODEL


type alias Error =
    String


type alias Model =
    { firstInput : String
    , secondInput : String
    , firstPersonResults : Result (List MyError) BdsmTestResults
    , secondPersonResults : Result (List MyError) BdsmTestResults
    , finalScore : Result String Float
    }


init : Model
init =
    { firstInput = ""
    , secondInput = ""
    , firstPersonResults = Err []
    , secondPersonResults = Err []
    , finalScore = Err ""
    }



-- UPDATE


type Msg
    = ParseFirst String
    | ParseSecond String
    | CalculateScore


update : Msg -> Model -> Model
update msg model =
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

        CalculateScore ->
            case ( model.firstPersonResults, model.secondPersonResults ) of
                ( Ok score1, Ok score2 ) ->
                    { model | finalScore = Ok <| calculateScore2 score1 score2 }

                ( Err _, Ok _ ) ->
                    { model | finalScore = Err "something's wrong with the first result" }

                ( Ok _, Err _ ) ->
                    { model | finalScore = Err "something's wrong with the second result" }

                ( Err _, Err _ ) ->
                    { model | finalScore = Err "something's wrong with the first and the second result" }



-- VIEW


view : Model -> Element Msg
view model =
    column [ centerX, paddingXY 0 200 ]
        [ row []
            [ viewInput ParseFirst "first input" "paste your results" model.firstPersonResults model.firstInput
            , viewInput ParseSecond "second input" "paste your results" model.secondPersonResults model.secondInput
            ]
        , button
            [ centerX
            , Background.color (rgb255 0x88 0xC0 0xD0)
            , Font.color backgroundColor
            , Border.rounded 5
            , padding 7
            ]
            { onPress = Just CalculateScore, label = text "calculate!" }
        , el
            [ centerX ]
          <|
            paragraph []
                [ case model.finalScore of
                    Ok result ->
                        result
                            |> (*) 100
                            |> round
                            |> String.fromInt
                            |> text

                    Err error ->
                        text error
                ]
        ]


viewInput : (String -> msg) -> String -> String -> Result (List MyError) BdsmTestResults -> String -> Element msg
viewInput onChange placeholderText labelText results value =
    column [ width (px 500) ]
        [ multiline [ Font.color backgroundColor ]
            { onChange = onChange
            , text = value
            , placeholder = Just <| placeholder [] (text placeholderText)
            , label = labelAbove [] (text labelText)
            , spellcheck = False
            }
        , paragraph []
            [ text (Debug.toString results) ]
        ]


red : Color
red =
    rgb255 255 0 0


backgroundColor : Color
backgroundColor =
    rgb255 0x2E 0x34 0x40


white : Color
white =
    rgb255 0xEC 0xEF 0xF4
