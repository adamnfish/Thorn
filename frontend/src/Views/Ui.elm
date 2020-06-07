module Views.Ui exposing (view)

import Browser
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Model exposing (..)
import Views.GameLogic exposing (isCreator)


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

                LobbyScreen playerOrder loadingStatus welcomeMessage ->
                    let
                        maybeGameStatus =
                            case Dict.get (getGid welcomeMessage.gameId) model.library of
                                Just (Playing game self _) ->
                                    Just
                                        { game = game
                                        , self = self
                                        }

                                Just (NotPlaying game self) ->
                                    Just
                                        { game = game
                                        , self = self
                                        }

                                Just (Waiting _ _) ->
                                    Nothing

                                Nothing ->
                                    Nothing
                    in
                    { title = "Waiting | Thorn"
                    , body = lobby model playerOrder loadingStatus welcomeMessage maybeGameStatus
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    }

                CurrentGameScreen game self welcomeMessage ->
                    { title = game.gameName ++ " | Thorn"
                    , body = currentGame model game self welcomeMessage
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
                        Playing game self welcomeMessage ->
                            row []
                                [ Input.button []
                                    { onPress = Just <| NavigateGame game self welcomeMessage
                                    , label = text <| self.screenName ++ " in " ++ game.gameName
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


currentGame : Model -> Game -> Self -> WelcomeMessage -> Element Msg
currentGame model game self welcomeMessage =
    el
        []
        (text "showing game")
