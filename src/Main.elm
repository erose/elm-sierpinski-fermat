module Main exposing (..)

import Array exposing (Array)
import Browser
import Debug
import Decimal exposing (Decimal)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)


type alias Triangle =
    Array String


type alias Fermat =
    String


type alias Color =
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



-- UPDATE


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



-- The line in the triangle which we multiply by the last Fermat number to get the current bottom
-- line of the triangle.


currentMultiplicand : Triangle -> String
currentMultiplicand triangle =
    let
        index =
            Array.length triangle - lastFermatNumberIndex triangle - 1
    in
    optimisticGet index triangle



-- The last Fermat number in the triangle. There will always be one.


lastFermatNumber : Triangle -> Fermat
lastFermatNumber triangle =
    optimisticGet (lastFermatNumberIndex triangle) triangle



-- The binary string representation of 2^(2^n) + 1.


nthFermatNumber : Int -> Fermat
nthFermatNumber n =
    "1" ++ String.repeat (2 ^ n - 1) "0" ++ "1"


isPowerOf2 : Int -> Bool
isPowerOf2 n =
    isInteger <| logBase 2 (toFloat n)



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



-- Converts a binary string (of zeroes and ones) to an infinite-precision decimal.


toDecimal : String -> Decimal
toDecimal s =
    let
        reversedChars =
            List.reverse <| String.toList s

        valuesAndChars =
            List.indexedMap (\i char -> ( Decimal.fromInt <| 2 ^ i, char )) reversedChars

        values =
            List.map Tuple.first <| List.filter (\( value, char ) -> char == '1') valuesAndChars
    in
    List.foldl Decimal.add Decimal.zero values


toDecimalString : String -> String
toDecimalString =
    Decimal.toString << toDecimal



-- VIEW


red : Color
red =
    "#8B0000"


blue : Color
blue =
    "#6495ED"


purple : Color
purple =
    "#4B0082"


green : Color
green =
    "#006400"


orange : Color
orange =
    "#FF8C00"


white : Color
white =
    "#FFFFFF"


view : Model -> Html Msg
view model =
    let
        rowDivs =
            Array.toList <| Array.indexedMap (\i line -> renderRowDiv model i line) model.triangle

        -- Show this button when there are enough rows to make it interesting.
        displayToggleBlocks =
            Array.length model.triangle > 8

        topPanelDiv =
            div [ style "position" "relative" ]
                [ rightSideDiv
                ]

        rightSideDiv =
            div [ style "position" "absolute", style "right" "1rem", style "display" "flex", style "flex-flow" "column" ]
                [ buttonsContainerDiv
                , div [ style "margin-top" "1rem" ] <|
                    List.concat
                        [ [ text "Fermat numbers used:" ]
                        , fermatNumberDivs
                        ]
                ]

        fermatNumberDivs =
            List.map (\line -> div [] [ text (line ++ " = " ++ toDecimalString line) ]) fermatNumbers

        fermatNumbers =
            List.map Tuple.second <| List.filter (\( i, line ) -> displayLineAtIndexAsFermatNumber i) <| Array.toIndexedList model.triangle

        -- The first element is always "1", and it's confusing to have that in there, so don't display it.
        -- The Fermat numbers are in the triangle at indices which are a power of 2.
        -- We don't want to display the last line, as that's technically added to the array before it becomes visible, and if it's displayed then it will be confusing.
        displayLineAtIndexAsFermatNumber i =
            (i /= 0) && isPowerOf2 i && (i /= Array.length model.triangle - 1)

        buttonsContainerDiv =
            div []
                (List.concat
                    [ [ button [ onClick Step ] [ text "Next" ] ]
                    , if displayToggleBlocks then
                        [ button [ onClick ToggleBlocks ] [ text "Toggle Blocks" ] ]

                      else
                        []
                    , [ button [ onClick Reset ] [ text "Reset" ] ]
                    ]
                )
    in
    div [ style "margin" "1rem", style "font-family" "monospace" ]
        [ topPanelDiv
        , div [ style "overflow-y" "auto", style "height" "25rem" ] rowDivs
        ]


renderRowDiv : Model -> Int -> String -> Html msg
renderRowDiv model index line =
    let
        isLastLine =
            index == (Array.length model.triangle - 1)

        colorStylesFor =
            colorStylesIfAllowedForModel model

        lineStyles =
            List.append [ style "white-space" "nowrap" ] <|
                if line == lastFermatNumber model.triangle then
                    colorStylesFor red

                else if line == currentMultiplicand model.triangle then
                    colorStylesFor blue

                else
                    []

        showingCalculation =
            model.showing == ShowingCalculation || model.showing == ShowingCalculationAndResult

        showingLastLine =
            model.showing == ShowingCalculationAndResult

        multiplicand =
            currentMultiplicand model.triangle

        fermat =
            lastFermatNumber model.triangle

        ( product, remainingFermatDigits ) =
            multiply fermat multiplicand

        -- Show it in pieces to illustrate how the multiplication worked.
        renderLastLineAsSpans =
            [ span (colorStylesFor blue) (renderStringAsSpans model multiplicand)
            , span (colorStylesFor red) (renderStringAsSpans model remainingFermatDigits)
            , span (colorStylesFor blue) (renderStringAsSpans model multiplicand)
            ]
    in
    div [ style "white-space" "nowrap" ]
        [ --  Content of the line.
          if isLastLine then
            div
                [ style "display" "inline-flex"
                , style "flex-flow" "column"
                , style "visibility"
                    (if not showingLastLine then
                        "hidden"

                     else
                        ""
                    )
                ]
                [ div lineStyles renderLastLineAsSpans
                , div [ style "text-align" "center" ] [ text <| toDecimalString product ]
                ]

          else
            span lineStyles <| renderStringAsSpans model line

        -- Calculation, if we are displaying one.
        , span []
            (if isLastLine && showingCalculation then
                [ span [] (renderCalculationSpans model) ]

             else
                []
            )
        ]


renderCalculationSpans : Model -> List (Html msg)
renderCalculationSpans model =
    let
        multiplicand =
            currentMultiplicand model.triangle

        fermat =
            lastFermatNumber model.triangle

        colorStylesFor =
            colorStylesIfAllowedForModel model
    in
    [ span [] [ text " = " ]
    , stackedDiv model (colorStylesFor red) fermat (toDecimalString fermat)
    , span [] [ text " x " ]
    , stackedDiv model (colorStylesFor blue) multiplicand (toDecimalString multiplicand)
    ]



-- Render an (inline) div that contains one string on the top and one string on the bottom.


stackedDiv : Model -> List (Html.Attribute msg) -> String -> String -> Html msg
stackedDiv model additionalStyles top bottom =
    div (List.append additionalStyles [ style "display" "inline-flex", style "flex-flow" "column" ])
        [ div [] (renderStringAsSpans model top)
        , div [ style "text-align" "center" ] [ text bottom ]
        ]



-- When displaying characters as blocks, we do not allow other color styling.


colorStylesIfAllowedForModel : Model -> Color -> List (Html.Attribute msg)
colorStylesIfAllowedForModel model color =
    if model.blocksOn then
        []

    else
        [ style "color" color ]


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
