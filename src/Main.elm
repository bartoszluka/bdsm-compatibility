module Main exposing (..)

import BdsmTestResults exposing (BdsmTestResults, MyError(..), calculateScoreSquared, parseInput, showError)
import Browser
import Browser.Events exposing (onResize)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button, labelAbove, multiline, placeholder)
import Hex
import Html exposing (Html)
import Svg
import Svg.Attributes as SvgAttr


main : Program Dimensions Model Msg
main =
    Browser.document
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = \model -> { title = "BDSM compatibility", body = [ viewLayout model ] }
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize SizeChanged


viewLayout : Model -> Html Msg
viewLayout model =
    layout [ Background.color backgroundColor, Font.color white ] (view model)



-- MODEL


type alias Error =
    String


type alias Dimensions =
    { width : Int, height : Int }


type alias Model =
    { firstInput : String
    , secondInput : String
    , firstPersonResults : Result (List MyError) BdsmTestResults
    , secondPersonResults : Result (List MyError) BdsmTestResults
    , finalScore : Result String Float
    , dimensions : Dimensions
    }


init : Dimensions -> ( Model, Cmd Msg )
init flags =
    ( { firstInput = ""
      , secondInput = ""
      , firstPersonResults = Err []
      , secondPersonResults = Err []
      , finalScore = Err ""
      , dimensions = { width = flags.width, height = flags.height }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ParseFirst String
    | ParseSecond String
    | CalculateScore
    | SizeChanged Int Int


isContentShort : String -> Bool
isContentShort content =
    (content |> String.trim |> String.lines |> List.length) <= 1


update : Msg -> Model -> Model
update msg model =
    case msg of
        ParseFirst newContent ->
            { model
                | firstInput = newContent
                , firstPersonResults =
                    if isContentShort newContent then
                        Err []

                    else
                        parseInput newContent
            }

        ParseSecond newContent ->
            { model
                | secondInput = newContent
                , secondPersonResults =
                    if isContentShort newContent then
                        Err []

                    else
                        parseInput newContent
            }

        CalculateScore ->
            case ( model.firstPersonResults, model.secondPersonResults ) of
                ( Ok score1, Ok score2 ) ->
                    { model | finalScore = Ok <| calculateScoreSquared score1 score2 }

                ( Err _, Ok _ ) ->
                    { model | finalScore = Err "something's wrong with the first result" }

                ( Ok _, Err _ ) ->
                    { model | finalScore = Err "something's wrong with the second result" }

                ( Err _, Err _ ) ->
                    { model | finalScore = Err "something's wrong with the first and the second result" }

        SizeChanged width height ->
            { model | dimensions = { width = width, height = height } }


type LayoutDirection
    = Horizontal
    | Vertical



-- VIEW


view : Model -> Element Msg
view model =
    let
        calculateButton =
            el [ padding 10, centerX ] <|
                button
                    [ Background.color (rgb255 0x88 0xC0 0xD0)
                    , Font.color backgroundColor
                    , Border.rounded 5
                    , padding 10
                    ]
                    { onPress = Just CalculateScore, label = text "calculate!" }

        viewResults fontSize =
            el [ centerX ] <|
                case model.finalScore of
                    Ok result ->
                        result
                            |> (*) 100
                            |> round
                            |> String.fromInt
                            |> (\s ->
                                    case s of
                                        "69" ->
                                            "your score is 69 (nice)"

                                        _ ->
                                            "your score is " ++ s
                               )
                            |> text
                            |> el [ Font.size fontSize, Font.color colorSuccess ]

                    Err error ->
                        paragraph [ Font.color colorWarning ] [ text error ]

        inputs layoutDirection =
            let
                input1 =
                    viewInput ParseFirst
                        [ "== Results from bdsmtest.org =="
                        , "98% Dominant"
                        , "98% Dominant"
                        , "94% Rigger"
                        , "94% Sadist"
                        , "..."
                        ]
                        "your results"
                        model.firstPersonResults
                        model.firstInput

                input2 =
                    viewInput ParseSecond
                        [ "== Results from bdsmtest.org =="
                        , "98% Submissive"
                        , "78% Masochist"
                        , "78% Rope bunny"
                        , "75% Experimentalist"
                        , "..."
                        ]
                        "your partner's results"
                        model.secondPersonResults
                        model.secondInput
            in
            case layoutDirection of
                Horizontal ->
                    row [ spacing 20, centerX ]
                        [ input1, input2 ]

                Vertical ->
                    column [ spacing 20, centerX ]
                        [ input1, input2 ]

        { class, orientation } =
            -- Debug.log "device" <|
            classifyDevice model.dimensions

        content direction base =
            let
                fontScale : Int -> Int
                fontScale =
                    modular base 1.25 >> round
            in
            column [ centerX, centerY, height fill, Font.size (fontScale 1) ]
                [ el [ Font.bold, Font.size (fontScale 3), padding 50, centerX ]
                    -- header
                    (text "BDSM compatibility test")
                , inputs direction
                , calculateButton
                , viewResults (fontScale 3)
                , row [ Element.alignBottom, centerX, padding 30 ]
                    -- footer
                    [ link [ Font.color (rgb255 0xAB 0xB9 0xCF), mouseOver [ Font.color white ] ]
                        { label =
                            row [ spacing 10 ]
                                [ githubIcon (fontScale 1)
                                , text "source code"
                                ]
                        , url = "https://github.com/bartoszluka/bdsm-compatibility"
                        }
                    ]
                ]
    in
    case ( class, orientation ) of
        ( Desktop, Landscape ) ->
            content Horizontal 17

        ( Phone, Portrait ) ->
            content Vertical 20

        ( Phone, Landscape ) ->
            content Horizontal 20

        ( Desktop, Portrait ) ->
            -- actually, a phone
            content Vertical 35

        ( BigDesktop, Portrait ) ->
            -- actually, a tall phone
            content Vertical 38

        ( BigDesktop, Landscape ) ->
            -- actually a tall phone in landscape
            content Horizontal 38

        ( Tablet, Portrait ) ->
            -- TODO: fix tablet
            content Vertical 40

        ( Tablet, Landscape ) ->
            -- TODO: fix tablet
            content Vertical 40


githubIcon : Int -> Element Msg
githubIcon iconSize =
    el [ width (px iconSize) ] <|
        html <|
            Svg.svg [ SvgAttr.fill (colorToHexString gray), SvgAttr.viewBox "0 0 496 512" ] [ Svg.path [ SvgAttr.d "M165.9 397.4c0 2-2.3 3.6-5.2 3.6-3.3.3-5.6-1.3-5.6-3.6 0-2 2.3-3.6 5.2-3.6 3-.3 5.6 1.3 5.6 3.6zm-31.1-4.5c-.7 2 1.3 4.3 4.3 4.9 2.6 1 5.6 0 6.2-2s-1.3-4.3-4.3-5.2c-2.6-.7-5.5.3-6.2 2.3zm44.2-1.7c-2.9.7-4.9 2.6-4.6 4.9.3 2 2.9 3.3 5.9 2.6 2.9-.7 4.9-2.6 4.6-4.6-.3-1.9-3-3.2-5.9-2.9zM244.8 8C106.1 8 0 113.3 0 252c0 110.9 69.8 205.8 169.5 239.2 12.8 2.3 17.3-5.6 17.3-12.1 0-6.2-.3-40.4-.3-61.4 0 0-70 15-84.7-29.8 0 0-11.4-29.1-27.8-36.6 0 0-22.9-15.7 1.6-15.4 0 0 24.9 2 38.6 25.8 21.9 38.6 58.6 27.5 72.9 20.9 2.3-16 8.8-27.1 16-33.7-55.9-6.2-112.3-14.3-112.3-110.5 0-27.5 7.6-41.3 23.6-58.9-2.6-6.5-11.1-33.3 2.6-67.9 20.9-6.5 69 27 69 27 20-5.6 41.5-8.5 62.8-8.5s42.8 2.9 62.8 8.5c0 0 48.1-33.6 69-27 13.7 34.7 5.2 61.4 2.6 67.9 16 17.7 25.8 31.5 25.8 58.9 0 96.5-58.9 104.2-114.8 110.5 9.2 7.9 17 22.9 17 46.4 0 33.7-.3 75.4-.3 83.6 0 6.5 4.6 14.4 17.3 12.1C428.2 457.8 496 362.9 496 252 496 113.3 383.5 8 244.8 8zM97.2 352.9c-1.3 1-1 3.3.7 5.2 1.6 1.6 3.9 2.3 5.2 1 1.3-1 1-3.3-.7-5.2-1.6-1.6-3.9-2.3-5.2-1zm-10.8-8.1c-.7 1.3.3 2.9 2.3 3.9 1.6 1 3.6.7 4.3-.7.7-1.3-.3-2.9-2.3-3.9-2-.6-3.6-.3-4.3.7zm32.4 35.6c-1.6 1.3-1 4.3 1.3 6.2 2.3 2.3 5.2 2.6 6.5 1 1.3-1.3.7-4.3-1.3-6.2-2.2-2.3-5.2-2.6-6.5-1zm-11.4-14.7c-1.6 1-1.6 3.6 0 5.9 1.6 2.3 4.3 3.3 5.6 2.3 1.6-1.3 1.6-3.9 0-6.2-1.4-2.3-4-3.3-5.6-2z" ] [] ]


gray : Color
gray =
    -- #d8dee9
    rgb255 0xD8 0xDE 0xE9


viewInput :
    (String -> msg)
    -> List String
    -> String
    -> Result (List MyError) BdsmTestResults
    -> String
    -> Element msg
viewInput onChange placeholderText labelText results value =
    column [ width fill ]
        [ multiline [ Font.color white, Background.color backgroundColor ]
            { onChange = onChange
            , text = value
            , placeholder = Just <| placeholder [] (column [] <| List.map text placeholderText)
            , label = labelAbove [] (text labelText)
            , spellcheck = False
            }
        , case results of
            Ok _ ->
                text "everything is correct :)"

            Err errors ->
                column [ Font.color colorWarning ] <| List.map (showError >> text) errors
        ]


colorSuccess : Color
colorSuccess =
    -- #8FBCBB
    rgb255 0x8F 0xBC 0xBB


colorWarning : Color
colorWarning =
    -- #EBCB8B
    rgb255 0xEB 0xCB 0x8B


redColor : Color
redColor =
    -- #ff0000
    rgb255 255 0 0


backgroundColor : Color
backgroundColor =
    -- #2e3440
    rgb255 0x2E 0x34 0x40


white : Color
white =
    -- #eceff4
    rgb255 0xEC 0xEF 0xF4


colorToHexString : Color -> String
colorToHexString color =
    let
        { red, green, blue } =
            toRgb color

        toHexString =
            (*) 255 >> round >> Hex.toString
    in
    [ red, green, blue ] |> List.map toHexString |> String.concat |> (\s -> "#" ++ s)
