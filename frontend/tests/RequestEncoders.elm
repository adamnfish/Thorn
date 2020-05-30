module RequestEncoders exposing (..)

import Expect exposing (Expectation, fail)
import Json.Decode
import Json.Encode
import Model exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Request encoders"
        [ describe "include correct operation field"
            [ test "createGame" <|
                \_ ->
                    let
                        json =
                            createGameEncoder
                                { screenName = "screen name"
                                , gameName = "game name"
                                }
                    in
                    expectOperationField "create-game" json
            , test "joinGame" <|
                \_ ->
                    let
                        json =
                            joinGameEncoder
                                { screenName = "screen name"
                                , gameCode = "abcde"
                                }
                    in
                    expectOperationField "join-game" json
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
                    in
                    expectOperationField "start-game" json
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
                    in
                    expectOperationField "place-disc" json
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
                    in
                    expectOperationField "bid" json
            , test "pass" <|
                \_ ->
                    let
                        json =
                            passEncoder
                                { playerId = Pid "pid"
                                , playerKey = Pkey "key"
                                , gameId = Gid "gid"
                                }
                    in
                    expectOperationField "pass" json
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
                    in
                    expectOperationField "flip" json
            , test "newRound" <|
                \_ ->
                    let
                        json =
                            newRoundEncoder
                                { playerId = Pid "pid"
                                , playerKey = Pkey "key"
                                , gameId = Gid "gid"
                                }
                    in
                    expectOperationField "new-round" json
            , test "reconnect" <|
                \_ ->
                    let
                        json =
                            reconnectEncoder
                                { playerId = Pid "pid"
                                , playerKey = Pkey "key"
                                , gameId = Gid "gid"
                                }
                    in
                    expectOperationField "reconnect" json
            , test "ping" <|
                \_ ->
                    let
                        json =
                            pingEncoder
                                { playerId = Pid "pid"
                                , playerKey = Pkey "key"
                                , gameId = Gid "gid"
                                }
                    in
                    expectOperationField "ping" json
            , test "wake" <|
                \_ ->
                    let
                        json =
                            wakeEncoder ()
                    in
                    expectOperationField "wake" json
            ]
        ]


expectOperationField : String -> Json.Encode.Value -> Expectation
expectOperationField expected json =
    let
        result =
            Json.Decode.decodeValue
                (Json.Decode.field "operation" Json.Decode.string)
                json
    in
    case result of
        Ok actual ->
            Expect.equal expected actual

        Err err ->
            fail <| Json.Decode.errorToString err
