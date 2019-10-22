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


type alias Model =
    { triangle : Triangle, blocksOn : Bool }


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, update = update, view = view }


initialModel : Model
initialModel =
    { triangle = Array.fromList [ "1", "11", "101" ]
    , blocksOn = False
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
            { model | triangle = Array.push nextTriangleLine model.triangle }

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


view : Model -> Html Msg
view model =
    let
        triangleDivs =
            Array.toList <| Array.indexedMap lineDiv model.triangle

        lineDiv index line =
            let
                isLastLine =
                    index == (Array.length model.triangle - 1)
            in
            div [ style "white-space" "nowrap" ] <|
                List.append (lineSpans line) <|
                    if isLastLine then
                        [ equationSpan ]

                    else
                        []

        equationSpan =
            span [ style "margin-left" "1rem" ] equationSpans

        equationSpans =
            let
                a =
                    lastFermatNumber model.triangle

                -- TODO: Explain.
                b =
                    optimisticGet (Array.length model.triangle - lastFermatNumberIndex model.triangle - 1) model.triangle
            in
            lineSpans (" = " ++ a ++ " x " ++ b)

        lineSpans line =
            List.map (\char -> span (charStyles char) [ text <| String.fromChar char ]) (String.toList line)

        charStyles char =
            if model.blocksOn then
                case char of
                    '0' ->
                        [ style "background-color" "blue" ]

                    '1' ->
                        [ style "background-color" "black", style "color" "white" ]

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
