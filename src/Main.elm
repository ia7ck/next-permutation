module Main exposing (main)

import Array exposing (..)
import Browser
import Html exposing (Html, button, div, li, strong, text, ul)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import List
import Tuple


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    List Int


init : Model
init =
    List.range 1 6



-- UPDATE


type Msg
    = Increment Int
    | Decrement Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment n ->
            f n nextPermutationCyclic model

        Decrement n ->
            f (factorial (List.length model) - n) nextPermutationCyclic model


f : Int -> (a -> a) -> a -> a
f n g x =
    -- g(g(g(...(g(x))...)))
    if n == 0 then
        x

    else
        f (n - 1) g (g x)


nextPermutationCyclic : List comparable -> List comparable
nextPermutationCyclic list =
    case
        List.map2 Tuple.pair list (List.drop 1 list)
            |> List.indexedMap Tuple.pair
            |> List.filter (\( _, ( x1, x2 ) ) -> x1 < x2)
            |> List.map (\( i, ( xi, _ ) ) -> ( i, xi ))
            |> List.maximum
    of
        Just ( i, xi ) ->
            let
                ( j, xj ) =
                    list
                        |> List.indexedMap Tuple.pair
                        |> List.filter (\( jj, y ) -> i < jj && xi < y)
                        |> List.maximum
                        |> Maybe.withDefault ( i, xi )

                tl =
                    list
                        |> List.indexedMap Tuple.pair
                        |> List.filter (\( jj, _ ) -> i < jj)
                        |> List.map
                            (\( jj, y ) ->
                                if j == jj then
                                    xi

                                else
                                    y
                            )
            in
            [ List.take i list, [ xj ], List.reverse tl ] |> List.concat

        Nothing ->
            List.reverse list


factorial : Int -> Int
factorial n =
    List.range 1 n |> List.product



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container", style "padding-top" "2rem" ]
        [ div [ class "row" ]
            [ div [ class "column text-center" ]
                [ ul [ class "list-none" ]
                    [ li []
                        [ button [ onClick (Decrement 1), class "button button-outline min-width-15" ]
                            [ text "Prev" ]
                        ]
                    , li []
                        [ button [ onClick (Decrement 10), class "button button-outline min-width-15" ]
                            [ text "Prev 10" ]
                        ]
                    , li []
                        [ button [ onClick (Decrement 100), class "button button-outline min-width-15" ]
                            [ text "Prev 100" ]
                        ]
                    ]
                ]
            , Html.p [ class "column", class "text-center font-size-2x" ]
                [ strong []
                    [ model |> List.map String.fromInt |> String.join " " |> text ]
                ]
            , div [ class "column", class "text-center" ]
                [ ul [ class "list-none" ]
                    [ li []
                        [ button [ onClick (Increment 1), class "button button-outline min-width-15" ]
                            [ text "Next" ]
                        ]
                    , li []
                        [ button [ onClick (Increment 10), class "button button-outline min-width-15" ]
                            [ text "Next 10" ]
                        ]
                    , li []
                        [ button [ onClick (Increment 100), class "button button-outline min-width-15" ]
                            [ text "Next 100" ]
                        ]
                    ]
                ]
            ]
        ]
