module MessageDecoders exposing (..)

import Expect exposing (Expectation, fail)
import Json.Decode
import Model exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Message decoders"
        [ describe "can decode valid message"
            [ test "welcome message" <|
                \_ ->
                    let
                        welcomeMessageStr =
                            """{"playerKey":"78125aa1-6f24-4744-a6cf-be21f3613d38","playerId":"6c4906a5-c44d-4a8e-9aeb-9dc548ac41e3","gameId":"9750edfd-a87e-4e2f-bef9-a76d989f18b4"}"""

                        result =
                            Json.Decode.decodeString messageDecoder welcomeMessageStr
                    in
                    case result of
                        Ok (Welcome data) ->
                            Expect.equal data <|
                                { playerKey = Pkey "78125aa1-6f24-4744-a6cf-be21f3613d38"
                                , playerId = Pid "6c4906a5-c44d-4a8e-9aeb-9dc548ac41e3"
                                , gameId = Gid "9750edfd-a87e-4e2f-bef9-a76d989f18b4"
                                }

                        Ok message ->
                            fail ("Expected Welcome message, got " ++ Debug.toString message)

                        Err err ->
                            fail <| Json.Decode.errorToString err
            , test "game status message" <|
                \_ ->
                    let
                        gameStatusMessageStr =
                            """{"self":{"screenName":"test","playerId":"6c4906a5-c44d-4a8e-9aeb-9dc548ac41e3","score":0,"placedDiscs":null,"roseCount":3,"hasThorn":true},"game":{"gameId":"9750edfd-a87e-4e2f-bef9-a76d989f18b4","gameName":"Test Game","creatorId":"6c4906a5-c44d-4a8e-9aeb-9dc548ac41e3","players":[{"screenName":"test","playerId":"6c4906a5-c44d-4a8e-9aeb-9dc548ac41e3","score":0,"discCount":4}],"round":null,"started":true}}"""

                        result =
                            Json.Decode.decodeString messageDecoder gameStatusMessageStr
                    in
                    case result of
                        Ok (GameStatus data) ->
                            allOf
                                [ Expect.equal data.self.hasThorn True
                                , Expect.equal data.self.roseCount 3
                                , Expect.equal data.self.placedDiscs Nothing
                                , Expect.equal data.self.screenName "test"
                                , Expect.equal data.self.playerId <| Pid "6c4906a5-c44d-4a8e-9aeb-9dc548ac41e3"
                                , Expect.equal data.game.gameId <| Gid "9750edfd-a87e-4e2f-bef9-a76d989f18b4"
                                , Expect.equal data.game.gameName <| "Test Game"
                                , Expect.equal data.game.started <| True
                                , Expect.equal (List.length data.game.players) 1
                                ]

                        Ok message ->
                            fail ("Expected Welcome message, got " ++ Debug.toString message)

                        Err err ->
                            fail <| Json.Decode.errorToString err
            , test "status message" <|
                \_ ->
                    let
                        gameStatusMessageStr =
                            """{"message":"message text"}"""

                        result =
                            Json.Decode.decodeString messageDecoder gameStatusMessageStr
                    in
                    case result of
                        Ok (Status data) ->
                            Expect.equal data.message "message text"

                        Ok message ->
                            fail ("Expected Welcome message, got " ++ Debug.toString message)

                        Err err ->
                            fail <| Json.Decode.errorToString err
            ]
        , describe "can decode failures"
            [ test "FailedAttempt with a single Failure" <|
                \_ ->
                    let
                        gameStatusMessageStr =
                            """{"failures":[{"friendlyMessage":"message text","statusCode":404,"context":null}]}"""

                        result =
                            Json.Decode.decodeString messageDecoder gameStatusMessageStr
                    in
                    case result of
                        Ok (FailedAttempt failures) ->
                            Expect.equal failures
                                [ { friendlyMessage = "message text"
                                  , statusCode = 404
                                  , context = Nothing
                                  }
                                ]

                        Ok message ->
                            fail ("Expected Welcome message, got " ++ Debug.toString message)

                        Err err ->
                            fail <| Json.Decode.errorToString err
            , test "FailedAttempt with multiple Failure" <|
                \_ ->
                    let
                        gameStatusMessageStr =
                            """{"failures":[{"friendlyMessage":"message text","statusCode":404,"context":null},{"friendlyMessage":"another message with text","statusCode":500}]}"""

                        result =
                            Json.Decode.decodeString messageDecoder gameStatusMessageStr
                    in
                    case result of
                        Ok (FailedAttempt failures) ->
                            Expect.equal failures
                                [ { friendlyMessage = "message text"
                                  , statusCode = 404
                                  , context = Nothing
                                  }
                                , { friendlyMessage = "another message with text"
                                  , statusCode = 500
                                  , context = Nothing
                                  }
                                ]

                        Ok message ->
                            fail ("Expected Welcome message, got " ++ Debug.toString message)

                        Err err ->
                            fail <| Json.Decode.errorToString err
            ]
        ]


allOf : List Expectation -> Expectation
allOf expectations =
    Expect.all (List.map always expectations) ()
