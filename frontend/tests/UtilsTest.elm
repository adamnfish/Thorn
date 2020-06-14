module UtilsTest exposing (..)

import Expect
import Test exposing (Test, describe, test)
import Utils exposing (reorderTo, reorderToBy, swapDown, swapUp)


all : Test
all =
    describe "utils"
        [ describe "reorderTo"
            [ test "returns empty list for empty input" <|
                \_ ->
                    reorderTo 1 [] |> Expect.equal []
            , test "returns provided list if the element does not exist" <|
                \_ ->
                    reorderTo 1 [ 2, 3 ] |> Expect.equal [ 2, 3 ]
            , test "order is preserved if the element is already first" <|
                \_ ->
                    reorderTo 1 [ 1, 2, 3 ] |> Expect.equal [ 1, 2, 3 ]
            , test "returns list re-ordered to start with the matching element" <|
                \_ ->
                    reorderTo 2 [ 1, 2, 3 ] |> Expect.equal [ 2, 3, 1 ]
            , test "returns correct order for more complex example" <|
                \_ ->
                    reorderTo 5 [ 1, 2, 3, 4, 5, 6 ] |> Expect.equal [ 5, 6, 1, 2, 3, 4 ]
            ]
        , describe "reorderToBy"
            [ test "returns empty list for empty input" <|
                \_ ->
                    reorderToBy .a 1 [] |> Expect.equal []
            , test "returns provided list if the element does not exist" <|
                \_ ->
                    reorderToBy .a 1 [ { a = 2 }, { a = 3 } ] |> Expect.equal [ { a = 2 }, { a = 3 } ]
            , test "order is preserved if the element is already first" <|
                \_ ->
                    reorderToBy .a 1 [ { a = 1 }, { a = 2 }, { a = 3 } ] |> Expect.equal [ { a = 1 }, { a = 2 }, { a = 3 } ]
            , test "returns list re-ordered to start with the matching element" <|
                \_ ->
                    reorderToBy .a 2 [ { a = 1 }, { a = 2 }, { a = 3 } ] |> Expect.equal [ { a = 2 }, { a = 3 }, { a = 1 } ]
            , test "returns correct order for more complex example" <|
                \_ ->
                    reorderToBy .a 5 [ { a = 1 }, { a = 2 }, { a = 3 }, { a = 4 }, { a = 5 }, { a = 6 } ] |> Expect.equal [ { a = 5 }, { a = 6 }, { a = 1 }, { a = 2 }, { a = 3 }, { a = 4 } ]
            ]
        , describe "swapDown"
            [ test "swaps the matching element 'down'" <|
                \_ ->
                    swapDown 2 [ 1, 2, 3 ] |> Expect.equal [ 1, 3, 2 ]
            , test "doesn't swap the element down if it is at the back already" <|
                \_ ->
                    swapDown 3 [ 1, 2, 3 ] |> Expect.equal [ 1, 2, 3 ]
            , test "swaps front element down" <|
                \_ ->
                    swapDown 1 [ 1, 2, 3 ] |> Expect.equal [ 2, 1, 3 ]
            , test "keeps existing list if el does not exist" <|
                \_ ->
                    swapDown 4 [ 1, 2, 3 ] |> Expect.equal [ 1, 2, 3 ]
            ]
        , describe "swapUp"
            [ test "swaps the matching element 'up'" <|
                \_ ->
                    swapUp 2 [ 1, 2, 3 ] |> Expect.equal [ 2, 1, 3 ]
            , test "doesn't swap the element up if it is at the front already" <|
                \_ ->
                    swapUp 1 [ 1, 2, 3 ] |> Expect.equal [ 1, 2, 3 ]
            , test "swaps back element up" <|
                \_ ->
                    swapUp 3 [ 1, 2, 3 ] |> Expect.equal [ 1, 3, 2 ]
            , test "keeps existing list if el does not exist" <|
                \_ ->
                    swapUp 4 [ 1, 2, 3 ] |> Expect.equal [ 1, 2, 3 ]
            ]
        ]
