module Main exposing (..)

import Array exposing (Array)
import Bitwise
import Browser
import Debug
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Triangle =
    Array String


type alias Model =
    { triangle : Triangle }


main : Program () Model Msg
main =
    let
        initialModel =
            { triangle = Array.fromList [ "1", "11", "101" ] }
    in
    Browser.sandbox { init = initialModel, update = update, view = view }


type Msg
    = Step


update : Msg -> Model -> Model
update msg model =
    let
        nextTriangleLine =
            nextLine model.triangle
    in
    case msg of
        Step ->
            { model | triangle = Array.push nextTriangleLine model.triangle }


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
nextLine lines =
    let
        numberOfLines =
            Array.length lines

        n =
            logBase 2 (toFloat <| Array.length lines)

        lastFermatNumberIndex =
            2 ^ floor n
    in
    if isInteger n then
        -- If the number of lines is a power of two, we've exhausted everything we can get from the
        -- Fermat numbers we currently have and need a new one.
        nthFermatNumber (floor n)

    else
        let
            lastFermatNumber =
                optimisticGet lastFermatNumberIndex lines
        in
        -- TODO: Explain.
        multiply lastFermatNumber <| optimisticGet (numberOfLines - lastFermatNumberIndex) lines



-- The binary string representation of 2^(2^n) + 1.


nthFermatNumber : Int -> String
nthFermatNumber n =
    "1" ++ String.repeat (2 ^ n - 1) "0" ++ "1"



-- "Multiplies" the integers represented by binary strings together, as long as one is a Fermat
-- number.


multiply : String -> String -> String
multiply fermat s =
    s ++ (String.dropLeft 1 <| String.dropRight (String.length s) fermat) ++ s



----fromInt <| toInt a * toInt b
---- Convert a string of zeroes and ones to an integer.
--toInt : String -> Int
--toInt characters =
--    let
--        toInt_ i list =
--            case ( i, list ) of
--                ( _, [] ) ->
--                    0
--                ( _, '0' :: xs ) ->
--                    0 + toInt_ (i + 1) xs
--                ( _, '1' :: xs ) ->
--                    2 ^ i + toInt_ (i + 1) xs
--                ( _, _ ) ->
--                    Debug.todo <| "toInt_ failed with i: " ++ String.fromInt i ++ "and list: " ++ String.fromList list
--    in
--    toInt_ 0 (List.reverse <| String.toList characters)
--fromInt : Int -> String
--fromInt n =
--    let
--        fromInt_ i acc =
--            case i of
--                0 ->
--                    '0' :: acc
--                1 ->
--                    '1' :: acc
--                _ ->
--                    fromInt_ (Bitwise.shiftRightBy 1 i) ((toChar <| modBy 2 i) :: acc)
--    in
--    String.fromList <| List.reverse <| fromInt_ n []
------ TODO: Do I have to do this this way?
--toChar : Int -> Char
--toChar n =
--    case String.toList <| String.fromInt n of
--        x :: _ ->
--            x
--        _ ->
--            Debug.todo "toChar failed with an empty list"


view : Model -> Html Msg
view model =
    let
        triangleDivs =
            Array.toList <| Array.map (\line -> div [] [ text line ]) model.triangle
    in
    div []
        [ button [ onClick Step ] [ text "Next" ]
        , div [] triangleDivs
        ]
