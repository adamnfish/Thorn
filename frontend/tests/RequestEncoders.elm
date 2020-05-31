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
                    createGameEncoder
                        { screenName = "screen name"
                        , gameName = "game name"
                        }
                        |> expectOperationField "create-game"
            , test "joinGame" <|
                \_ ->
                    joinGameEncoder
                        { screenName = "screen name"
                        , gameCode = "abcde"
                        }
                        |> expectOperationField "join-game"
            , test "startGame" <|
                \_ ->
                    startGameEncoder
                        { playerId = Pid "pid"
                        , playerKey = Pkey "key"
                        , gameId = Gid "gid"
                        , playerOrder = []
                        }
                        |> expectOperationField "start-game"
            , test "placeDisc" <|
                \_ ->
                    placeDiscEncoder
                        { playerId = Pid "pid"
                        , playerKey = Pkey "key"
                        , gameId = Gid "gid"
                        , disc = Thorn
                        }
                        |> expectOperationField "place-disc"
            , test "bid" <|
                \_ ->
                    bidEncoder
                        { playerId = Pid "pid"
                        , playerKey = Pkey "key"
                        , gameId = Gid "gid"
                        , count = 1
                        }
                        |> expectOperationField "bid"
            , test "pass" <|
                \_ ->
                    passEncoder
                        { playerId = Pid "pid"
                        , playerKey = Pkey "key"
                        , gameId = Gid "gid"
                        }
                        |> expectOperationField "pass"
            , test "flip" <|
                \_ ->
                    flipEncoder
                        { playerId = Pid "pid"
                        , playerKey = Pkey "key"
                        , gameId = Gid "gid"
                        , stack = Pid "another-pid"
                        }
                        |> expectOperationField "flip"
            , test "newRound" <|
                \_ ->
                    newRoundEncoder
                        { playerId = Pid "pid"
                        , playerKey = Pkey "key"
                        , gameId = Gid "gid"
                        }
                        |> expectOperationField "new-round"
            , test "reconnect" <|
                \_ ->
                    reconnectEncoder
                        { playerId = Pid "pid"
                        , playerKey = Pkey "key"
                        , gameId = Gid "gid"
                        }
                        |> expectOperationField "reconnect"
            , test "ping" <|
                \_ ->
                    pingEncoder
                        { playerId = Pid "pid"
                        , playerKey = Pkey "key"
                        , gameId = Gid "gid"
                        }
                        |> expectOperationField "ping"
            , test "wake" <|
                \_ ->
                    wakeEncoder
                        ()
                        |> expectOperationField "wake"
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
