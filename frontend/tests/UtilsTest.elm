module UtilsTest exposing (..)

import Expect
import Test exposing (Test, describe, test)
import Utils exposing (reorderTo, reorderToBy)


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
        ]
