module Msg exposing (..)

import Json.Decode
import Model exposing (..)
import Ports exposing (sendMessage)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ServerMessage json ->
            let
                messageResult =
                    Json.Decode.decodeValue messageDecoder json
            in
            case messageResult of
                Err _ ->
                    -- TODO: think about messaging / logging
                    ( model, Cmd.none )

                Ok (FailedAttempt failures) ->
                    -- TODO: think about error messaging / logging
                    ( model, Cmd.none )

                Ok (Status statusMessage) ->
                    -- TODO: think about messaging / logging
                    ( model, Cmd.none )

                Ok (Welcome welcomeMessage) ->
                    ( { model
                        | current =
                            Just <|
                                Waiting welcomeMessage
                      }
                    , Cmd.none
                    )

                Ok (GameStatus gameStatusMessage) ->
                    case model.current of
                        Just (Playing _ _ welcome) ->
                            ( { model
                                | current =
                                    Just <|
                                        Playing gameStatusMessage.game gameStatusMessage.self welcome
                              }
                            , Cmd.none
                            )

                        _ ->
                            -- TODO: think about error reporting / UX!
                            ( model, Cmd.none )

        SendMessageTmpTest ->
            ( model
            , sendCreateGame
                { screenName = "example temp screen name"
                , gameName = "example temp game name"
                }
            )

        SocketConnect ->
            case model.current of
                Just (Waiting welcome) ->
                    ( model
                    , sendReconnect welcome
                    )

                Just (Playing _ _ welcome) ->
                    ( model
                    , sendReconnect welcome
                    )

                Nothing ->
                    ( model, Cmd.none )


sendCreateGame : CreateGame -> Cmd msg
sendCreateGame createGame =
    sendMessage <|
        createGameEncoder createGame


sendJoinGame : JoinGame -> Cmd msg
sendJoinGame joinGame =
    sendMessage <| joinGameEncoder joinGame


sendStartGame : StartGame -> Cmd msg
sendStartGame startGame =
    sendMessage <| startGameEncoder startGame


sendPlaceDisc : PlaceDisc -> Cmd msg
sendPlaceDisc placeDisc =
    sendMessage <| placeDiscEncoder placeDisc


sendBid : Bid -> Cmd msg
sendBid bid =
    sendMessage <| bidEncoder bid


sendPass : Pass -> Cmd msg
sendPass pass =
    sendMessage <| passEncoder pass


sendFlip : Flip -> Cmd msg
sendFlip flip =
    sendMessage <| flipEncoder flip


sendNewRound : NewRound -> Cmd msg
sendNewRound newRound =
    sendMessage <| newRoundEncoder newRound


sendReconnect : Reconnect -> Cmd msg
sendReconnect reconnect =
    sendMessage <| reconnectEncoder reconnect


sendPing : Ping -> Cmd msg
sendPing ping =
    sendMessage <| pingEncoder ping


sendWake : () -> Cmd msg
sendWake _ =
    sendMessage <| wakeEncoder ()
