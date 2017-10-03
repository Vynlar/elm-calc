module Calc exposing (..)

import Html exposing (..)
import String
import List
import Result


input =
    "5 5 + 2 * 3 4 + * 8" |> String.words


main =
    input |> calculate [] |> toString |> text


calculate : List Float -> List String -> Result String Float
calculate stack inputs =
    case List.head inputs of
        Just input ->
            case String.toFloat input of
                Ok number ->
                    let
                        newInput =
                            List.tail inputs

                        newStack =
                            number :: stack
                    in
                        case newInput of
                            Just items ->
                                calculate newStack items

                            Nothing ->
                                Err "You're probably missing an operator"

                Err err ->
                    let
                        doOperation func =
                            let
                                items =
                                    List.take 2 stack

                                newStack =
                                    func items :: List.drop 2 stack

                                newInput =
                                    List.tail inputs
                            in
                                case newInput of
                                    Just tail ->
                                        calculate newStack tail

                                    Nothing ->
                                        case List.head stack of
                                            Just result ->
                                                Ok result

                                            Nothing ->
                                                Err "There was a problem"
                    in
                        case input of
                            "+" ->
                                doOperation List.sum

                            "*" ->
                                doOperation List.product

                            _ ->
                                Err "invalid operator"

        Nothing ->
            case List.head stack of
                Just result ->
                    Ok result

                Nothing ->
                    Err "There was a problem"
