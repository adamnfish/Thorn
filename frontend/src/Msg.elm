module Msg exposing (..)

import Browser.Dom
import Dict
import Json.Decode
import List.Extra
import Model exposing (..)
import Ports exposing (reportError, sendMessage)
import Task
import Time
import Utils exposing (flip)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick time ->
            let
                filteredErrors =
                    List.filter
                        (\uiErr ->
                            Time.posixToMillis uiErr.time + 6000 < Time.posixToMillis time
                        )
                        model.errors
            in
            ( { model
                | now = time
                , errors = filteredErrors
              }
            , Cmd.none
            )

        OnResize ->
            ( model
            , Task.perform Resized Browser.Dom.getViewport
            )

        Resized viewport ->
            ( { model
                | viewport = Just viewport
              }
            , Cmd.none
            )

        ServerMessage json ->
            let
                messageResult =
                    Json.Decode.decodeValue messageDecoder json
            in
            case messageResult of
                Err err ->
                    ( displayFailure model
                        { friendlyMessage = "Error communicating with server"
                        , statusCode = 500
                        , context = Nothing
                        }
                    , reportError <| Json.Decode.errorToString err
                    )

                Ok (FailedAttempt failures) ->
                    let
                        updatedModel =
                            displayFailures model failures
                    in
                    case model.ui of
                        CreateGameScreen gameName screenName AwaitingMessage ->
                            ( { updatedModel
                                | ui = CreateGameScreen gameName screenName NotLoading
                              }
                            , Cmd.none
                            )

                        JoinGameScreen gameCode screenName AwaitingMessage ->
                            ( { updatedModel
                                | ui = JoinGameScreen gameCode screenName NotLoading
                              }
                            , Cmd.none
                            )

                        LobbyScreen playerOrder AwaitingMessage welcomeMessage ->
                            ( { updatedModel
                                | ui = LobbyScreen playerOrder NotLoading welcomeMessage
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( updatedModel, Cmd.none )

                Ok (Status statusMessage) ->
                    ( displayFailure model
                        { friendlyMessage = statusMessage.message
                        , statusCode = 200
                        , context = Nothing
                        }
                    , Cmd.none
                    )

                Ok (Welcome welcomeMessage) ->
                    let
                        gid =
                            getGid welcomeMessage.gameId

                        newLibrary =
                            Dict.update gid
                                (\maybeGameStatus ->
                                    case maybeGameStatus of
                                        Nothing ->
                                            Just <| Waiting welcomeMessage []

                                        Just (Playing gameStatus _) ->
                                            Just <| Playing gameStatus welcomeMessage

                                        Just (Waiting _ playerOrder) ->
                                            Just <| Waiting welcomeMessage playerOrder

                                        Just (NotPlaying gameStatus) ->
                                            Just <| Playing gameStatus welcomeMessage
                                )
                                model.library
                    in
                    case model.ui of
                        CreateGameScreen _ _ _ ->
                            ( { model
                                | ui = LobbyScreen [] NotLoading welcomeMessage
                                , library = newLibrary
                              }
                            , Cmd.none
                            )

                        JoinGameScreen _ _ _ ->
                            ( { model
                                | ui = LobbyScreen [] NotLoading welcomeMessage
                                , library = newLibrary
                              }
                            , Cmd.none
                            )

                        HomeScreen ->
                            -- TODO: think about messaging / logging
                            ( { model
                                | library = newLibrary
                              }
                            , Cmd.none
                            )

                        CurrentGameScreen _ _ ->
                            -- TODO: think about messaging / logging
                            ( { model
                                | library = newLibrary
                              }
                            , Cmd.none
                            )

                        LobbyScreen _ _ _ ->
                            -- TODO: think about messaging / logging
                            ( { model
                                | library = newLibrary
                              }
                            , Cmd.none
                            )

                Ok (GameStatus gameStatusMessage) ->
                    let
                        gid =
                            getGid gameStatusMessage.game.gameId

                        maybeWelcomeMessage =
                            case Dict.get gid model.library of
                                Just (Waiting welcomeMessage _) ->
                                    Just welcomeMessage

                                Just (Playing _ welcomeMessage) ->
                                    Just welcomeMessage

                                Just (NotPlaying _) ->
                                    Nothing

                                Nothing ->
                                    Nothing
                    in
                    case maybeWelcomeMessage of
                        Just welcomeMessage ->
                            let
                                newLibrary =
                                    Dict.insert gid
                                        (Playing gameStatusMessage welcomeMessage)
                                        model.library

                                modelWithLib =
                                    { model | library = newLibrary }
                            in
                            case model.ui of
                                HomeScreen ->
                                    ( modelWithLib, Cmd.none )

                                CurrentGameScreen _ _ ->
                                    ( { modelWithLib
                                        | ui = CurrentGameScreen gameStatusMessage welcomeMessage
                                      }
                                    , Cmd.none
                                    )

                                CreateGameScreen _ _ _ ->
                                    -- message is for another game, just update lib in background
                                    ( modelWithLib, Cmd.none )

                                JoinGameScreen _ _ _ ->
                                    -- message is for another game, just update lib in background
                                    ( modelWithLib, Cmd.none )

                                LobbyScreen playerOrder loadingStatus _ ->
                                    if gameStatusMessage.game.started then
                                        ( { modelWithLib
                                            | ui = CurrentGameScreen gameStatusMessage welcomeMessage
                                          }
                                        , Cmd.none
                                        )

                                    else
                                        let
                                            newPlayerOrder =
                                                includeAllPlayers playerOrder gameStatusMessage.game.players
                                        in
                                        ( { modelWithLib
                                            | ui = LobbyScreen newPlayerOrder loadingStatus welcomeMessage
                                          }
                                        , Cmd.none
                                        )

                        Nothing ->
                            let
                                newLibrary =
                                    Dict.insert gid
                                        (NotPlaying gameStatusMessage)
                                        model.library

                                modelWithLib =
                                    { model | library = newLibrary }
                            in
                            -- received a status message for a game we don't have a welcome for
                            ( modelWithLib, Cmd.none )

        SocketConnect ->
            let
                newModel =
                    { model | connected = True }
            in
            case model.ui of
                CurrentGameScreen _ welcome ->
                    ( newModel
                    , sendReconnect welcome
                    )

                LobbyScreen _ _ welcome ->
                    ( newModel
                    , sendReconnect welcome
                    )

                _ ->
                    ( newModel, Cmd.none )

        SocketDisconnect ->
            ( { model | connected = False }
            , Cmd.none
            )

        NavigateHome ->
            ( { model | ui = HomeScreen }
            , Cmd.none
            )

        NavigateGame gameStatus welcome ->
            ( { model | ui = CurrentGameScreen gameStatus welcome }
            , Cmd.none
            )

        NavigateCreateGame ->
            ( { model
                | ui = CreateGameScreen "" "" NotLoading
              }
            , Cmd.none
            )

        InputCreateGame gameName screenName loadingStatus ->
            ( { model
                | ui = CreateGameScreen gameName screenName loadingStatus
              }
            , Cmd.none
            )

        SubmitCreateGame gameName screenName ->
            ( { model
                | ui = CreateGameScreen gameName screenName AwaitingMessage
              }
            , sendCreateGame
                { gameName = gameName
                , screenName = screenName
                }
            )

        NavigateJoinGame ->
            ( { model
                | ui = JoinGameScreen "" "" NotLoading
              }
            , Cmd.none
            )

        InputJoinGame gameCode screenName loadingStatus ->
            ( { model
                | ui = JoinGameScreen gameCode screenName loadingStatus
              }
            , Cmd.none
            )

        SubmitJoinGame gameCode screenName ->
            ( { model
                | ui = JoinGameScreen gameCode screenName AwaitingMessage
              }
            , sendJoinGame
                { gameCode = gameCode
                , screenName = screenName
                }
            )

        InputReorderPlayers playerOrder ->
            case model.ui of
                LobbyScreen _ loadingStatus welcomeMessage ->
                    ( { model | ui = LobbyScreen playerOrder loadingStatus welcomeMessage }
                    , Cmd.none
                    )

                _ ->
                    ( displayFailure model
                        { friendlyMessage = "Reordering player can only be done from the Lobby"
                        , statusCode = 400
                        , context = Nothing
                        }
                    , Cmd.none
                    )

        SubmitStartGame ->
            case model.ui of
                LobbyScreen playerOrder _ welcomeMessage ->
                    ( { model | ui = LobbyScreen playerOrder AwaitingMessage welcomeMessage }
                    , sendStartGame
                        { gameId = welcomeMessage.gameId
                        , playerId = welcomeMessage.playerId
                        , playerKey = welcomeMessage.playerKey
                        , playerOrder = List.map .playerId playerOrder
                        }
                    )

                _ ->
                    ( displayFailure model
                        { friendlyMessage = "Game can only be started from the Lobby"
                        , statusCode = 400
                        , context = Nothing
                        }
                    , Cmd.none
                    )


displayFailure : Model -> Failure -> Model
displayFailure model failure =
    let
        error =
            { message = failure
            , time = model.now
            }
    in
    { model
        | errors = error :: model.errors
    }


displayFailures : Model -> List Failure -> Model
displayFailures model failures =
    List.foldl (flip displayFailure) model failures


includeAllPlayers : List Player -> List Player -> List Player
includeAllPlayers p1s p2s =
    List.Extra.uniqueBy (\p -> getPid p.playerId) <|
        List.append p1s p2s


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
