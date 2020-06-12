module Views.Ui exposing (view)

import Browser
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Maybe.Extra
import Model exposing (..)
import Views.GameLogic exposing (isActive, isCreator)


type alias Page =
    { title : String
    , body : Element Msg
    , nav : Element Msg
    }


view : Model -> Browser.Document Msg
view model =
    let
        page : Page
        page =
            case model.ui of
                HomeScreen ->
                    { title = "Thorn"
                    , body = home model
                    , nav = Element.none
                    }

                CreateGameScreen gameName screenName loadingStatus ->
                    { title = "Create game | Thorn"
                    , body = createGame model gameName screenName loadingStatus
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    }

                JoinGameScreen gameCode screenName loadingStatus ->
                    { title = "Join game | Thorn"
                    , body = joinGame model gameCode screenName loadingStatus
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    }

                LobbyScreen playerOrder welcomeMessage loadingStatus ->
                    let
                        maybeGameStatus =
                            case Dict.get (getGid welcomeMessage.gameId) model.library of
                                Just (Playing gameStatus _) ->
                                    Just gameStatus

                                Just (NotPlaying gameStatus) ->
                                    Just gameStatus

                                Just (Waiting _ _) ->
                                    Nothing

                                Nothing ->
                                    Nothing
                    in
                    { title = "Waiting | Thorn"
                    , body = lobby model playerOrder loadingStatus welcomeMessage maybeGameStatus
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    }

                DisplayGameScreen gameStatus welcomeMessage ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , body = currentGame model gameStatus welcomeMessage
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    }

                PlaceDiscScreen maybeDisc gameStatus welcomeMessage loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , body = placeDisc model gameStatus welcomeMessage maybeDisc
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    }

                DiscOrBidScreen maybeDiscOrBid gameStatus welcomeMessage loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , body = currentGame model gameStatus welcomeMessage
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    }

                BidScreen maybeInt gameStatus welcomeMessage loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , body = currentGame model gameStatus welcomeMessage
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    }

                FlipScreen maybePlayerId gameStatus welcomeMessage loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , body = currentGame model gameStatus welcomeMessage
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    }
    in
    { title = page.title
    , body =
        [ layout [] <|
            Element.column []
                [ page.nav
                , page.body
                ]
        ]
    }


nav : List ( Msg, String ) -> Element Msg
nav navEntries =
    Element.row [] <|
        List.map
            (\( msg, label ) ->
                Input.button []
                    { onPress = Just msg
                    , label = text label
                    }
            )
            navEntries


home : Model -> Element Msg
home model =
    let
        availableGames =
            List.map
                (\gameInProgress ->
                    case gameInProgress of
                        Playing gameStatus welcomeMessage ->
                            row []
                                [ Input.button []
                                    { onPress = Just <| NavigateGame gameStatus welcomeMessage
                                    , label = text <| gameStatus.self.screenName ++ " in " ++ gameStatus.game.gameName
                                    }
                                ]

                        _ ->
                            Element.none
                )
            <|
                Dict.values model.library
    in
    el [] <|
        column
            []
            [ Input.button []
                { onPress = Just NavigateCreateGame
                , label = text "Create game"
                }
            , Input.button []
                { onPress = Just NavigateJoinGame
                , label = text "Join game"
                }
            , column [] availableGames
            ]


createGame : Model -> String -> String -> LoadingStatus -> Element Msg
createGame model gameName screenName loadingStatus =
    column
        []
        [ Input.text []
            { onChange = \newGameName -> InputCreateGame newGameName screenName loadingStatus
            , text = gameName
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Game name"
            }
        , Input.text []
            { onChange = \newScreenName -> InputCreateGame gameName newScreenName loadingStatus
            , text = screenName
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Screen name"
            }
        , Input.button []
            { onPress = Just <| SubmitCreateGame gameName screenName
            , label = text "Create game"
            }
        ]


joinGame : Model -> String -> String -> LoadingStatus -> Element Msg
joinGame model gameCode screenName loadingStatus =
    column
        []
        [ Input.text []
            { onChange = \newGameName -> InputJoinGame newGameName screenName loadingStatus
            , text = gameCode
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Game code"
            }
        , Input.text []
            { onChange = \newScreenName -> InputJoinGame gameCode newScreenName loadingStatus
            , text = screenName
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "Screen name"
            }
        , Input.button []
            { onPress = Just <| SubmitJoinGame gameCode screenName
            , label = text "Join game"
            }
        ]


lobby : Model -> List Player -> LoadingStatus -> WelcomeMessage -> Maybe GameStatusMessage -> Element Msg
lobby model playerOrder loadingStatus welcomeMessage maybeGameStatus =
    let
        playersEl =
            if List.isEmpty playerOrder then
                text "Waiting for other players"

            else
                column [] <|
                    List.map
                        (\player ->
                            text player.screenName
                        )
                        playerOrder

        startGameEl =
            case maybeGameStatus of
                Just gameStatus ->
                    if isCreator gameStatus.game gameStatus.self then
                        if List.length playerOrder >= 3 then
                            Input.button []
                                { onPress = Just SubmitStartGame
                                , label = text "Start game"
                                }

                        else
                            paragraph []
                                [ text "There must be at least 3 players to start the game" ]

                    else
                        Element.none

                Nothing ->
                    Element.none
    in
    column
        []
        [ text "Lobby"
        , text "Game code"
        , text <| gameCode welcomeMessage.gameId
        , playersEl
        , startGameEl
        ]


placeDisc : Model -> GameStatusMessage -> WelcomeMessage -> Maybe Disc -> Element Msg
placeDisc model gameStatus welcomeMessage maybeDisc =
    let
        roseButton =
            if gameStatus.self.roseCount > 0 then
                Input.button []
                    { onPress = Just <| InputPlaceDisc Rose
                    , label = text "Rose"
                    }

            else
                Element.none

        thornButton =
            if gameStatus.self.hasThorn then
                Input.button []
                    { onPress = Just <| InputPlaceDisc Thorn
                    , label = text "Thorn"
                    }

            else
                Element.none

        buttons =
            case maybeDisc of
                Just disc ->
                    [ Input.button []
                        { onPress = Just <| InputRemovePlaceDisc
                        , label = text "Clear"
                        }
                    , Input.button []
                        { onPress = Just <| SubmitPlaceDisc disc
                        , label = text "Submit"
                        }
                    ]

                Nothing ->
                    [ roseButton, thornButton ]
    in
    column []
        [ paragraph []
            [ text "Place initial disc" ]
        , column [] buttons
        ]


currentGame : Model -> GameStatusMessage -> WelcomeMessage -> Element Msg
currentGame model gameStatus welcomeMessage =
    let
        roundInfo =
            case gameStatus.game.round of
                Just (InitialDiscs initialDiscs) ->
                    1

                Just (Placing placing) ->
                    1

                Just (Bidding bidding) ->
                    1

                Just (Flipping flipping) ->
                    1

                Just (Finished finished) ->
                    1

                Nothing ->
                    1
    in
    el
        []
        (text "showing game")
