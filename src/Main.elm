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


type alias Fermat =
    String


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

            ( result, _ ) =
                multiply (lastFermatNumber triangle) lineToMultiply
        in
        result


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


currentMultiplicandIndex : Triangle -> Int
currentMultiplicandIndex triangle =
    Array.length triangle - lastFermatNumberIndex triangle - 1


currentMultiplicand : Triangle -> String
currentMultiplicand triangle =
    optimisticGet (currentMultiplicandIndex triangle) triangle


lastFermatNumber : Triangle -> Fermat
lastFermatNumber triangle =
    optimisticGet (lastFermatNumberIndex triangle) triangle



-- The binary string representation of 2^(2^n) + 1.


nthFermatNumber : Int -> Fermat
nthFermatNumber n =
    "1" ++ String.repeat (2 ^ n - 1) "0" ++ "1"



-- "Multiplies" the integers represented by binary strings together, as long as one is a Fermat
-- number. Returns (the result of the multiplication as a string, the digits of fermat that remain
-- in the result).


multiply : Fermat -> String -> ( String, String )
multiply fermat s =
    let
        remainingFermatDigits =
            String.dropLeft 1 <| String.dropRight (String.length s) fermat
    in
    ( s ++ remainingFermatDigits ++ s, remainingFermatDigits )



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
        rowDivs =
            Array.toList <| Array.indexedMap (\index line -> renderRowDiv model index line) model.triangle
    in
    div []
        [ button [ onClick Step ] [ text "Next" ]
        , button [ onClick ToggleBlocks ] [ text "Toggle Blocks" ]
        , button [ onClick Reset ] [ text "Reset" ]
        , div [ style "font-family" "monospace", style "margin" "1rem" ] rowDivs
        ]


renderRowDiv : Model -> Int -> String -> Html msg
renderRowDiv model index line =
    let
        isLastLine =
            index == (Array.length model.triangle - 1)

        -- When displaying characters as blocks, we do not allow emphasis styling.
        emphasisColorStyles color =
            if model.blocksOn then
                []

            else
                [ style "color" color ]

        lineStyles =
            List.append [ style "white-space" "nowrap" ] <|
                if isLastLine && not showingLastLine then
                    [ style "visibility" "hidden" ]

                else if index == lastFermatNumberIndex model.triangle then
                    emphasisColorStyles red

                else if index == currentMultiplicandIndex model.triangle then
                    emphasisColorStyles blue

                else
                    []

        showingCalculation =
            model.showing == ShowingCalculation || model.showing == ShowingCalculationAndResult

        showingLastLine =
            model.showing == ShowingCalculationAndResult

        -- Show it in pieces to illustrate how the multiplication worked.
        renderLastLineAsSpans =
            let
                s =
                    currentMultiplicand model.triangle

                fermat =
                    lastFermatNumber model.triangle

                ( _, remainingFermatDigits ) =
                    multiply fermat s
            in
            [ span (emphasisColorStyles blue) (renderStringAsSpans model s)
            , span (emphasisColorStyles red) (renderStringAsSpans model remainingFermatDigits)
            , span (emphasisColorStyles blue) (renderStringAsSpans model s)
            ]
    in
    div [ style "white-space" "nowrap" ]
        [ --  Content of the line.
          if isLastLine then
            span lineStyles renderLastLineAsSpans

          else
            span lineStyles <| renderStringAsSpans model line

        -- Calculation, if we are displaying one.
        , span []
            (if isLastLine && showingCalculation then
                [ span [] <| renderCalculationSpans model ]

             else
                []
            )
        ]


renderCalculationSpans : Model -> List (Html msg)
renderCalculationSpans model =
    let
        s =
            currentMultiplicand model.triangle

        -- TODO: Duplicate.
        -- When displaying characters as blocks, we do not allow emphasis styling.
        emphasisColorStyles color =
            if model.blocksOn then
                []

            else
                [ style "color" color ]

        fermat =
            lastFermatNumber model.triangle
    in
    [ span [] [ text " = " ]
    , span (emphasisColorStyles blue) (renderStringAsSpans model fermat)
    , span [] [ text " x " ]
    , span (emphasisColorStyles red) (renderStringAsSpans model s)
    ]


renderStringAsSpans : Model -> String -> List (Html msg)
renderStringAsSpans model s =
    let
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

        charToSpan char =
            span (blockCharStyles char) [ text <| String.fromChar char ]
    in
    List.map charToSpan <| String.toList s
