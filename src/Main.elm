module Main exposing (..)

import Array exposing (Array)
import Bitwise
import Browser
import Debug
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)


type alias Triangle =
    Array String


type Showing
    = ShowingCalculationAndResult
    | ShowingCalculation
    | ShowingNeither


type alias Model =
    { triangle : Triangle, blocksOn : Bool, showing : Showing }


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, update = update, view = view }


initialModel : Model
initialModel =
    { triangle = Array.fromList [ "1", "11", "101" ]
    , blocksOn = False
    , showing = ShowingNeither
    }


type Msg
    = Step
    | ToggleBlocks
    | Reset


update : Msg -> Model -> Model
update msg model =
    let
        nextTriangleLine =
            nextLine model.triangle
    in
    case msg of
        Step ->
            case model.showing of
                ShowingNeither ->
                    { model | showing = ShowingCalculation }

                ShowingCalculation ->
                    { model | showing = ShowingCalculationAndResult }

                ShowingCalculationAndResult ->
                    { model | triangle = Array.push nextTriangleLine model.triangle, showing = ShowingNeither }

        ToggleBlocks ->
            { model | blocksOn = not model.blocksOn }

        Reset ->
            initialModel


optimisticGet : Int -> Array a -> a
optimisticGet n array =
    case Array.get n array of
        Just element ->
            element

        Nothing ->
            Debug.todo ("optimisticGet found Nothing for index: " ++ String.fromInt n)


isInteger : Float -> Bool
isInteger f =
    (toFloat <| truncate f) - f == 0


nextLine : Triangle -> String
nextLine triangle =
    let
        numberOfLines =
            Array.length triangle

        n =
            logBase 2 (toFloat numberOfLines)
    in
    if isInteger n then
        -- If the number of lines is a power of two, we've exhausted everything we can get from the
        -- Fermat numbers we currently have and need a new one.
        nthFermatNumber (floor n)

    else
        let
            lineToMultiply =
                optimisticGet (numberOfLines - lastFermatNumberIndex triangle) triangle
        in
        multiply (lastFermatNumber triangle) lineToMultiply


lastFermatNumberIndex : Triangle -> Int
lastFermatNumberIndex triangle =
    let
        n =
            logBase 2 (toFloat <| Array.length triangle)
    in
    if isInteger n then
        2 ^ (floor n - 1)

    else
        2 ^ floor n



-- TODO: Explain.


lineThatWasMultipliedIndex : Triangle -> Int
lineThatWasMultipliedIndex triangle =
    Array.length triangle - lastFermatNumberIndex triangle - 1


lastFermatNumber : Triangle -> String
lastFermatNumber triangle =
    optimisticGet (lastFermatNumberIndex triangle) triangle



-- The binary string representation of 2^(2^n) + 1.


nthFermatNumber : Int -> String
nthFermatNumber n =
    "1" ++ String.repeat (2 ^ n - 1) "0" ++ "1"



-- "Multiplies" the integers represented by binary strings together, as long as one is a Fermat
-- number.


multiply : String -> String -> String
multiply fermat s =
    s ++ (String.dropLeft 1 <| String.dropRight (String.length s) fermat) ++ s



-- VIEW


red =
    "#8B0000"


blue =
    "#6495ED"


purple =
    "#4B0082"


green =
    "#006400"


orange =
    "#FF8C00"


white =
    "#FFFFFF"


view : Model -> Html Msg
view model =
    let
        triangleDivs =
            Array.toList <| Array.indexedMap rowDiv model.triangle

        rowDiv index line =
            let
                isLastLine =
                    index == (Array.length model.triangle - 1)

                lineStyles =
                    List.append [ style "white-space" "nowrap" ] <|
                        if isLastLine && not showingLastLine then
                            [ style "visibility" "hidden" ]

                        else if model.blocksOn then
                            []

                        else if index == lastFermatNumberIndex model.triangle then
                            [ style "color" red ]

                        else if index == lineThatWasMultipliedIndex model.triangle then
                            [ style "color" blue ]

                        else if isLastLine then
                            [ style "color" purple ]

                        else
                            []

                showingCalculation =
                    model.showing == ShowingCalculation || model.showing == ShowingCalculationAndResult

                showingLastLine =
                    model.showing == ShowingCalculationAndResult
            in
            div [ style "white-space" "nowrap" ]
                [ span lineStyles (renderStringAsSpans line)
                , span []
                    (if isLastLine && showingCalculation then
                        [ span [] calculationSpans ]

                     else
                        []
                    )
                ]

        calculationSpans =
            let
                a =
                    optimisticGet (lineThatWasMultipliedIndex model.triangle) model.triangle

                b =
                    lastFermatNumber model.triangle
            in
            [ span [] [ text " = " ]
            , span
                (if not model.blocksOn then
                    [ style "color" blue ]

                 else
                    []
                )
                (renderStringAsSpans a)
            , span [] [ text " x " ]
            , span
                (if not model.blocksOn then
                    [ style "color" red ]

                 else
                    []
                )
                (renderStringAsSpans b)
            ]

        renderStringAsSpans s =
            List.map (\char -> span (blockCharStyles char) [ text <| String.fromChar char ]) (String.toList s)

        blockCharStyles char =
            if model.blocksOn then
                case char of
                    '0' ->
                        [ style "background-color" orange ]

                    '1' ->
                        [ style "background-color" green, style "color" white ]

                    _ ->
                        []

            else
                []
    in
    div []
        [ button [ onClick Step ] [ text "Next" ]
        , button [ onClick ToggleBlocks ] [ text "Toggle Blocks" ]
        , button [ onClick Reset ] [ text "Reset" ]
        , div [ style "font-family" "monospace", style "margin" "1rem" ] triangleDivs
        ]
