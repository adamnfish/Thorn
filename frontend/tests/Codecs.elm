module Codecs exposing (..)

import Expect
import Json.Decode
import Model exposing (..)
import Test exposing (..)
import TestUtils exposing (okResult)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Message encoders"
        [ describe "include correct operation field"
            [ test "createGame" <|
                \_ ->
                    let
                        json =
                            createGameEncoder
                                { screenName = "screen name"
                                , gameName = "game name"
                                }

                        operationResult =
                            Json.Decode.decodeValue
                                (Json.Decode.field "operation" Json.Decode.string)
                                json
                    in
                    okResult (Expect.equal "create-game") operationResult
            , test "joinGame" <|
                \_ ->
                    let
                        json =
                            joinGameEncoder
                                { screenName = "screen name"
                                , gameCode = "abcde"
                                }

                        operationResult =
                            Json.Decode.decodeValue
                                (Json.Decode.field "operation" Json.Decode.string)
                                json
                    in
                    okResult (Expect.equal "join-game") operationResult
            , test "startGame" <|
                \_ ->
                    let
                        json =
                            startGameEncoder
                                { playerId = Pid "pid"
                                , playerKey = Pkey "key"
                                , gameId = Gid "gid"
                                , playerOrder = []
                                }

                        operationResult =
                            Json.Decode.decodeValue
                                (Json.Decode.field "operation" Json.Decode.string)
                                json
                    in
                    okResult (Expect.equal "start-game") operationResult
            , test "placeDisc" <|
                \_ ->
                    let
                        json =
                            placeDiscEncoder
                                { playerId = Pid "pid"
                                , playerKey = Pkey "key"
                                , gameId = Gid "gid"
                                , disc = Thorn
                                }

                        operationResult =
                            Json.Decode.decodeValue
                                (Json.Decode.field "operation" Json.Decode.string)
                                json
                    in
                    okResult (Expect.equal "place-disc") operationResult
            , test "bid" <|
                \_ ->
                    let
                        json =
                            bidEncoder
                                { playerId = Pid "pid"
                                , playerKey = Pkey "key"
                                , gameId = Gid "gid"
                                , count = 1
                                }

                        operationResult =
                            Json.Decode.decodeValue
                                (Json.Decode.field "operation" Json.Decode.string)
                                json
                    in
                    okResult (Expect.equal "bid") operationResult
            , test "pass" <|
                \_ ->
                    let
                        json =
                            passEncoder
                                { playerId = Pid "pid"
                                , playerKey = Pkey "key"
                                , gameId = Gid "gid"
                                }

                        operationResult =
                            Json.Decode.decodeValue
                                (Json.Decode.field "operation" Json.Decode.string)
                                json
                    in
                    okResult (Expect.equal "pass") operationResult
            , test "flip" <|
                \_ ->
                    let
                        json =
                            flipEncoder
                                { playerId = Pid "pid"
                                , playerKey = Pkey "key"
                                , gameId = Gid "gid"
                                , stack = Pid "another-pid"
                                }

                        operationResult =
                            Json.Decode.decodeValue
                                (Json.Decode.field "operation" Json.Decode.string)
                                json
                    in
                    okResult (Expect.equal "flip") operationResult
            , test "newRound" <|
                \_ ->
                    let
                        json =
                            newRoundEncoder
                                { playerId = Pid "pid"
                                , playerKey = Pkey "key"
                                , gameId = Gid "gid"
                                }

                        operationResult =
                            Json.Decode.decodeValue
                                (Json.Decode.field "operation" Json.Decode.string)
                                json
                    in
                    okResult (Expect.equal "new-round") operationResult
            , test "reconnect" <|
                \_ ->
                    let
                        json =
                            reconnectEncoder
                                { playerId = Pid "pid"
                                , playerKey = Pkey "key"
                                , gameId = Gid "gid"
                                }

                        operationResult =
                            Json.Decode.decodeValue
                                (Json.Decode.field "operation" Json.Decode.string)
                                json
                    in
                    okResult (Expect.equal "reconnect") operationResult
            , test "ping" <|
                \_ ->
                    let
                        json =
                            pingEncoder
                                { playerId = Pid "pid"
                                , playerKey = Pkey "key"
                                , gameId = Gid "gid"
                                }

                        operationResult =
                            Json.Decode.decodeValue
                                (Json.Decode.field "operation" Json.Decode.string)
                                json
                    in
                    okResult (Expect.equal "ping") operationResult
            , test "wake" <|
                \_ ->
                    let
                        json =
                            wakeEncoder ()

                        operationResult =
                            Json.Decode.decodeValue
                                (Json.Decode.field "operation" Json.Decode.string)
                                json
                    in
                    okResult (Expect.equal "wake") operationResult
            ]
        ]



--[ test "Addition" <|
--    \_ ->
--        Expect.equal 10 (3 + 7)
--, test "String.left" <|
--    \_ ->
--        Expect.equal "a" (String.left 1 "abcdefg")
--, test "This test should fail" <|
--    \_ ->
--        Expect.fail "failed as expected!"
--]
