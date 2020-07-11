module Msg exposing (..)

import Browser.Dom
import Dict exposing (Dict)
import GameLogic exposing (isCreator, selfIsActive)
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
                            Time.posixToMillis uiErr.time + 6000 > Time.posixToMillis time
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

                        LobbyScreen playerOrder welcomeMessage AwaitingMessage ->
                            ( { updatedModel
                                | ui = LobbyScreen playerOrder welcomeMessage NotLoading
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( updatedModel, Cmd.none )

                Ok (Status statusMessage) ->
                    if statusMessage.message == "ok" then
                        ( model, Cmd.none )

                    else
                        ( displayFailure model
                            { friendlyMessage = statusMessage.message
                            , statusCode = 200
                            , context = Nothing
                            }
                        , Cmd.none
                        )

                Ok (Welcome welcomeMessage) ->
                    welcomeMessageUpdate model welcomeMessage

                Ok (GameStatus gameStatusMessage) ->
                    gameStatusMessageUpdate model gameStatusMessage

        SocketConnect ->
            let
                newModel =
                    { model | connected = True }
            in
            case model.ui of
                DisplayGameScreen _ welcome ->
                    ( newModel
                    , sendPing welcome
                    )

                LobbyScreen _ welcome _ ->
                    ( newModel
                    , sendPing welcome
                    )

                PlaceDiscScreen _ _ welcome _ ->
                    ( newModel
                    , sendPing welcome
                    )

                DiscOrBidScreen _ _ welcome _ ->
                    ( newModel
                    , sendPing welcome
                    )

                BidOrPassScreen _ _ welcome _ ->
                    ( newModel
                    , sendPing welcome
                    )

                FlipScreen _ _ welcome _ ->
                    ( newModel
                    , sendPing welcome
                    )

                HomeScreen ->
                    ( newModel, Cmd.none )

                CreateGameScreen _ _ _ ->
                    ( newModel, Cmd.none )

                JoinGameScreen _ _ _ ->
                    ( newModel, Cmd.none )

        SocketDisconnect ->
            ( { model | connected = False }
            , Cmd.none
            )

        NavigateHome ->
            ( { model | ui = HomeScreen }
            , Cmd.none
            )

        NavigateGame gameStatus welcomeMessage ->
            ( { model | ui = uiForGameState gameStatus welcomeMessage }
            , sendPing
                { gameId = welcomeMessage.gameId
                , playerId = welcomeMessage.playerId
                , playerKey = welcomeMessage.playerKey
                }
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
                LobbyScreen currentOrder loadingStatus welcomeMessage ->
                    let
                        -- deal with edge case of new players arriving after ordering
                        newOrder =
                            includeAllPlayers playerOrder currentOrder
                    in
                    ( { model | ui = LobbyScreen newOrder loadingStatus welcomeMessage }
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
                LobbyScreen playerOrder welcomeMessage _ ->
                    ( { model | ui = LobbyScreen playerOrder welcomeMessage AwaitingMessage }
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

        InputPlaceDisc disc ->
            case model.ui of
                PlaceDiscScreen _ gameStatus welcomeMessage loadingStatus ->
                    ( { model | ui = PlaceDiscScreen (Just disc) gameStatus welcomeMessage loadingStatus }
                    , Cmd.none
                    )

                DiscOrBidScreen _ gameStatus welcomeMessage loadingStatus ->
                    ( { model | ui = DiscOrBidScreen (DiscOrBidDisc disc) gameStatus welcomeMessage loadingStatus }
                    , Cmd.none
                    )

                _ ->
                    ( displayFailure model
                        { friendlyMessage = "You can only place discs when it is your turn"
                        , statusCode = 400
                        , context = Nothing
                        }
                    , Cmd.none
                    )

        InputRemovePlaceDisc ->
            case model.ui of
                PlaceDiscScreen _ gameStatus welcomeMessage loadingStatus ->
                    ( { model | ui = PlaceDiscScreen Nothing gameStatus welcomeMessage loadingStatus }
                    , Cmd.none
                    )

                DiscOrBidScreen (DiscOrBidDisc _) gameStatus welcomeMessage loadingStatus ->
                    ( { model | ui = DiscOrBidScreen DiscOrBidNoSelection gameStatus welcomeMessage loadingStatus }
                    , Cmd.none
                    )

                DiscOrBidScreen _ _ _ _ ->
                    -- don't remove bid selection from "remove place disc" message
                    ( model, Cmd.none )

                _ ->
                    ( displayFailure model
                        { friendlyMessage = "You can only place discs when it is your turn"
                        , statusCode = 400
                        , context = Nothing
                        }
                    , Cmd.none
                    )

        SubmitPlaceDisc disc ->
            case model.ui of
                PlaceDiscScreen _ gameStatus welcomeMessage _ ->
                    ( { model | ui = PlaceDiscScreen (Just disc) gameStatus welcomeMessage AwaitingMessage }
                    , sendPlaceDisc
                        { gameId = welcomeMessage.gameId
                        , playerId = welcomeMessage.playerId
                        , playerKey = welcomeMessage.playerKey
                        , disc = disc
                        }
                    )

                DiscOrBidScreen _ gameStatus welcomeMessage _ ->
                    ( { model | ui = DiscOrBidScreen (DiscOrBidDisc disc) gameStatus welcomeMessage AwaitingMessage }
                    , sendPlaceDisc
                        { gameId = welcomeMessage.gameId
                        , playerId = welcomeMessage.playerId
                        , playerKey = welcomeMessage.playerKey
                        , disc = disc
                        }
                    )

                _ ->
                    ( displayFailure model
                        { friendlyMessage = "You can only place discs when it is your turn"
                        , statusCode = 400
                        , context = Nothing
                        }
                    , Cmd.none
                    )

        InputBid bid ->
            case model.ui of
                DiscOrBidScreen _ gameStatus welcomeMessage loadingStatus ->
                    ( { model | ui = DiscOrBidScreen (DiscOrBidBid bid) gameStatus welcomeMessage loadingStatus }
                    , Cmd.none
                    )

                BidOrPassScreen _ gameStatus welcomeMessage loadingStatus ->
                    ( { model | ui = BidOrPassScreen (BidOrPassBid bid) gameStatus welcomeMessage loadingStatus }
                    , Cmd.none
                    )

                _ ->
                    ( displayFailure model
                        { friendlyMessage = "You can only bid when it is your turn"
                        , statusCode = 400
                        , context = Nothing
                        }
                    , Cmd.none
                    )

        InputRemoveBid ->
            case model.ui of
                DiscOrBidScreen (DiscOrBidBid _) gameStatus welcomeMessage loadingStatus ->
                    ( { model | ui = DiscOrBidScreen DiscOrBidNoSelection gameStatus welcomeMessage loadingStatus }
                    , Cmd.none
                    )

                DiscOrBidScreen _ _ _ _ ->
                    -- don't remove disc selection from "remove bid" message
                    ( model, Cmd.none )

                BidOrPassScreen _ gameStatus welcomeMessage loadingStatus ->
                    ( { model | ui = BidOrPassScreen BidOrPassNoSelection gameStatus welcomeMessage loadingStatus }
                    , Cmd.none
                    )

                _ ->
                    ( displayFailure model
                        { friendlyMessage = "You can only place discs when it is your turn"
                        , statusCode = 400
                        , context = Nothing
                        }
                    , Cmd.none
                    )

        SubmitBid bid ->
            case model.ui of
                DiscOrBidScreen _ gameStatus welcomeMessage _ ->
                    ( { model | ui = DiscOrBidScreen (DiscOrBidBid bid) gameStatus welcomeMessage AwaitingMessage }
                    , sendBid
                        { gameId = welcomeMessage.gameId
                        , playerId = welcomeMessage.playerId
                        , playerKey = welcomeMessage.playerKey
                        , count = bid
                        }
                    )

                BidOrPassScreen _ gameStatus welcomeMessage _ ->
                    ( { model | ui = BidOrPassScreen (BidOrPassBid bid) gameStatus welcomeMessage AwaitingMessage }
                    , sendBid
                        { gameId = welcomeMessage.gameId
                        , playerId = welcomeMessage.playerId
                        , playerKey = welcomeMessage.playerKey
                        , count = bid
                        }
                    )

                _ ->
                    ( displayFailure model
                        { friendlyMessage = "You can only place discs when it is your turn"
                        , statusCode = 400
                        , context = Nothing
                        }
                    , Cmd.none
                    )

        InputPass ->
            case model.ui of
                BidOrPassScreen _ gameStatus welcomeMessage _ ->
                    ( { model | ui = BidOrPassScreen BidOrPassPass gameStatus welcomeMessage AwaitingMessage }
                    , Cmd.none
                    )

                _ ->
                    ( displayFailure model
                        { friendlyMessage = "You can only bid when it is your turn"
                        , statusCode = 400
                        , context = Nothing
                        }
                    , Cmd.none
                    )

        InputRemovePass ->
            case model.ui of
                BidOrPassScreen _ gameStatus welcomeMessage _ ->
                    ( { model | ui = BidOrPassScreen BidOrPassNoSelection gameStatus welcomeMessage AwaitingMessage }
                    , Cmd.none
                    )

                _ ->
                    ( displayFailure model
                        { friendlyMessage = "You can only bid when it is your turn"
                        , statusCode = 400
                        , context = Nothing
                        }
                    , Cmd.none
                    )

        SubmitPass ->
            case model.ui of
                BidOrPassScreen _ gameStatus welcomeMessage _ ->
                    ( { model | ui = BidOrPassScreen BidOrPassPass gameStatus welcomeMessage AwaitingMessage }
                    , sendPass
                        { gameId = welcomeMessage.gameId
                        , playerId = welcomeMessage.playerId
                        , playerKey = welcomeMessage.playerKey
                        }
                    )

                _ ->
                    ( displayFailure model
                        { friendlyMessage = "You can only bid when it is your turn"
                        , statusCode = 400
                        , context = Nothing
                        }
                    , Cmd.none
                    )

        InputFlip stackId ->
            case model.ui of
                FlipScreen _ gameStatus welcomeMessage loadingStatus ->
                    ( { model | ui = FlipScreen (Just stackId) gameStatus welcomeMessage loadingStatus }
                    , Cmd.none
                    )

                _ ->
                    ( displayFailure model
                        { friendlyMessage = "You can only flip discs when it is your turn to do so"
                        , statusCode = 400
                        , context = Nothing
                        }
                    , Cmd.none
                    )

        InputRemoveFlip ->
            case model.ui of
                FlipScreen _ gameStatus welcomeMessage loadingStatus ->
                    ( { model | ui = FlipScreen Nothing gameStatus welcomeMessage loadingStatus }
                    , Cmd.none
                    )

                _ ->
                    ( displayFailure model
                        { friendlyMessage = "You can only flip discs when it is your turn to do so"
                        , statusCode = 400
                        , context = Nothing
                        }
                    , Cmd.none
                    )

        SubmitFlip stackId ->
            case model.ui of
                FlipScreen _ gameStatus welcomeMessage _ ->
                    ( { model | ui = FlipScreen (Just stackId) gameStatus welcomeMessage AwaitingMessage }
                    , sendFlip
                        { gameId = welcomeMessage.gameId
                        , playerId = welcomeMessage.playerId
                        , playerKey = welcomeMessage.playerKey
                        , stack = stackId
                        }
                    )

                _ ->
                    ( displayFailure model
                        { friendlyMessage = "You can only flip discs when it is your turn to do so"
                        , statusCode = 400
                        , context = Nothing
                        }
                    , Cmd.none
                    )

        SubmitNewRound ->
            case model.ui of
                DisplayGameScreen gameStatus welcomeMessage ->
                    case ( gameStatus.game.round, isCreator gameStatus.game gameStatus.self ) of
                        ( Just (Finished _), True ) ->
                            ( { model | ui = DisplayGameScreen gameStatus welcomeMessage }
                            , sendNewRound
                                { gameId = welcomeMessage.gameId
                                , playerId = welcomeMessage.playerId
                                , playerKey = welcomeMessage.playerKey
                                }
                            )

                        ( _, True ) ->
                            ( displayFailure model
                                { friendlyMessage = "You can only start a new round after the current one is finished"
                                , statusCode = 400
                                , context = Nothing
                                }
                            , Cmd.none
                            )

                        ( _, False ) ->
                            ( displayFailure model
                                { friendlyMessage = "Only the game's creator can start a new round"
                                , statusCode = 400
                                , context = Nothing
                                }
                            , Cmd.none
                            )

                _ ->
                    ( displayFailure model
                        { friendlyMessage = "You can only flip discs when it is your turn to do so"
                        , statusCode = 400
                        , context = Nothing
                        }
                    , Cmd.none
                    )


welcomeMessageUpdate : Model -> WelcomeMessage -> ( Model, Cmd Msg )
welcomeMessageUpdate model welcomeMessage =
    let
        newLibrary =
            Dict.update (getGid welcomeMessage.gameId)
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
                | ui = LobbyScreen [] welcomeMessage NotLoading
                , library = newLibrary
              }
            , Cmd.none
            )

        JoinGameScreen _ _ _ ->
            ( { model
                | ui = LobbyScreen [] welcomeMessage NotLoading
                , library = newLibrary
              }
            , Cmd.none
            )

        _ ->
            -- background update
            ( { model
                | library = newLibrary
              }
            , Cmd.none
            )


gameStatusMessageUpdate : Model -> GameStatusMessage -> ( Model, Cmd Msg )
gameStatusMessageUpdate model gameStatusMessage =
    let
        nowActive =
            selfIsActive gameStatusMessage

        newLibrary =
            Dict.update (getGid gameStatusMessage.game.gameId)
                (\maybeGameStatus ->
                    case maybeGameStatus of
                        Nothing ->
                            Just <| NotPlaying gameStatusMessage

                        Just (Playing _ welcomeMessage) ->
                            Just <| Playing gameStatusMessage welcomeMessage

                        Just (Waiting welcomeMessage _) ->
                            Just <| Playing gameStatusMessage welcomeMessage

                        Just (NotPlaying _) ->
                            Just <| NotPlaying gameStatusMessage
                )
                model.library
    in
    case model.ui of
        HomeScreen ->
            ( { model
                | library = newLibrary
              }
            , Cmd.none
            )

        CreateGameScreen _ _ _ ->
            ( { model
                | library = newLibrary
              }
            , Cmd.none
            )

        JoinGameScreen _ _ _ ->
            ( { model
                | library = newLibrary
              }
            , Cmd.none
            )

        LobbyScreen playerOrder welcomeMessage loadingStatus ->
            if welcomeMessage.gameId == gameStatusMessage.game.gameId then
                if gameStatusMessage.game.started then
                    ( { model
                        | ui = uiForGameState gameStatusMessage welcomeMessage
                        , library = newLibrary
                      }
                    , Cmd.none
                    )

                else
                    let
                        newPlayerOrder =
                            includeAllPlayers playerOrder gameStatusMessage.game.players
                    in
                    ( { model
                        | ui = LobbyScreen newPlayerOrder welcomeMessage loadingStatus
                        , library = newLibrary
                      }
                    , Cmd.none
                    )

            else
                ( { model | library = newLibrary }
                , Cmd.none
                )

        PlaceDiscScreen maybeDisc _ welcomeMessage loadingStatus ->
            if welcomeMessage.gameId == gameStatusMessage.game.gameId then
                if nowActive then
                    case gameStatusMessage.game.round of
                        Just (InitialDiscs _) ->
                            -- server state matches client, update data and keep same screen
                            ( { model
                                | ui = PlaceDiscScreen maybeDisc gameStatusMessage welcomeMessage loadingStatus
                                , library = newLibrary
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( { model
                                | library = newLibrary
                                , ui = uiForGameState gameStatusMessage welcomeMessage
                              }
                            , reportError "PlaceDiscScreen jumped to another in-game state unexpectedly"
                            )

                else
                    -- server says not active so let's just display the game
                    ( { model
                        | ui = uiForGameState gameStatusMessage welcomeMessage
                        , library = newLibrary
                      }
                    , Cmd.none
                    )

            else
                -- background game update
                ( { model | library = newLibrary }
                , Cmd.none
                )

        DiscOrBidScreen maybeDiscOrBid _ welcomeMessage loadingStatus ->
            if welcomeMessage.gameId == gameStatusMessage.game.gameId then
                if nowActive then
                    case gameStatusMessage.game.round of
                        Just (Placing _) ->
                            -- server state matches client, update data and keep same screen
                            ( { model
                                | ui = DiscOrBidScreen maybeDiscOrBid gameStatusMessage welcomeMessage loadingStatus
                                , library = newLibrary
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( { model
                                | library = newLibrary
                                , ui = uiForGameState gameStatusMessage welcomeMessage
                              }
                            , reportError "DiscOrBidScreen jumped to another in-game state unexpectedly"
                            )

                else
                    -- server says not active so let's just display the game
                    ( { model
                        | ui = uiForGameState gameStatusMessage welcomeMessage
                        , library = newLibrary
                      }
                    , Cmd.none
                    )

            else
                -- background game update
                ( { model | library = newLibrary }
                , Cmd.none
                )

        BidOrPassScreen maybeBid _ welcomeMessage loadingStatus ->
            if welcomeMessage.gameId == gameStatusMessage.game.gameId then
                if nowActive then
                    case gameStatusMessage.game.round of
                        Just (Bidding _) ->
                            -- server state matches client, update data and keep same screen
                            ( { model
                                | ui = BidOrPassScreen maybeBid gameStatusMessage welcomeMessage loadingStatus
                                , library = newLibrary
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( { model
                                | library = newLibrary
                                , ui = uiForGameState gameStatusMessage welcomeMessage
                              }
                            , reportError "BidScreen jumped to another in-game state unexpectedly"
                            )

                else
                    -- server says not active so let's just display the game
                    ( { model
                        | ui = uiForGameState gameStatusMessage welcomeMessage
                        , library = newLibrary
                      }
                    , Cmd.none
                    )

            else
                -- background game update
                ( { model | library = newLibrary }
                , Cmd.none
                )

        FlipScreen maybeStackId _ welcomeMessage loadingStatus ->
            if welcomeMessage.gameId == gameStatusMessage.game.gameId then
                if nowActive then
                    case gameStatusMessage.game.round of
                        Just (Flipping _) ->
                            -- server state matches client, update data and keep same screen
                            ( { model
                                | ui = FlipScreen Nothing gameStatusMessage welcomeMessage NotLoading
                                , library = newLibrary
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( { model
                                | library = newLibrary
                                , ui = uiForGameState gameStatusMessage welcomeMessage
                              }
                            , reportError "FlipScreen jumped to another in-game state unexpectedly"
                            )

                else
                    -- server says not active so let's just display the game
                    ( { model
                        | ui = uiForGameState gameStatusMessage welcomeMessage
                        , library = newLibrary
                      }
                    , Cmd.none
                    )

            else
                -- background game update
                ( { model | library = newLibrary }
                , Cmd.none
                )

        DisplayGameScreen _ welcomeMessage ->
            ( { model
                | library = newLibrary
                , ui = uiForGameState gameStatusMessage welcomeMessage
              }
            , Cmd.none
            )


uiForGameState : GameStatusMessage -> WelcomeMessage -> UI
uiForGameState gameStatusMessage welcomeMessage =
    case gameStatusMessage.game.round of
        Just (InitialDiscs _) ->
            if selfIsActive gameStatusMessage then
                PlaceDiscScreen Nothing gameStatusMessage welcomeMessage NotLoading

            else
                DisplayGameScreen gameStatusMessage welcomeMessage

        Just (Placing _) ->
            if selfIsActive gameStatusMessage then
                DiscOrBidScreen DiscOrBidNoSelection gameStatusMessage welcomeMessage NotLoading

            else
                DisplayGameScreen gameStatusMessage welcomeMessage

        Just (Bidding _) ->
            if selfIsActive gameStatusMessage then
                BidOrPassScreen BidOrPassNoSelection gameStatusMessage welcomeMessage NotLoading

            else
                DisplayGameScreen gameStatusMessage welcomeMessage

        Just (Flipping _) ->
            if selfIsActive gameStatusMessage then
                FlipScreen Nothing gameStatusMessage welcomeMessage NotLoading

            else
                DisplayGameScreen gameStatusMessage welcomeMessage

        Just (Finished _) ->
            DisplayGameScreen gameStatusMessage welcomeMessage

        Nothing ->
            LobbyScreen gameStatusMessage.game.players welcomeMessage NotLoading


displayFailure : Model -> Failure -> Model
displayFailure model failure =
    let
        error =
            { failure = failure
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


sendPing : Ping -> Cmd msg
sendPing ping =
    sendMessage <| pingEncoder ping


sendWake : () -> Cmd msg
sendWake _ =
    sendMessage <| wakeEncoder ()
