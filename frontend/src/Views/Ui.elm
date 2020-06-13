module Views.Ui exposing (view)

import Browser
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import GameLogic exposing (isActive, isCreator, minBid, numberOfPlacedDiscs)
import List.Extra
import Maybe.Extra
import Model exposing (..)
import Utils exposing (reorderToBy)
import Views.Styles exposing (buttonStyles, colourError, size3, textColourLight)


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

                DisplayGameScreen gameStatus _ ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , body = currentGame model gameStatus
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    }

                PlaceDiscScreen maybeDisc gameStatus _ loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , body = placeDisc model gameStatus maybeDisc
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    }

                DiscOrBidScreen maybeSelection gameStatus _ loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , body = discOrBid model gameStatus maybeSelection
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    }

                BidOrPassScreen maybeInt gameStatus _ loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , body = bidOrPass model gameStatus maybeInt
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    }

                FlipScreen maybeStack gameStatus _ loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , body = flip model gameStatus maybeStack
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    }
    in
    { title = page.title
    , body =
        [ layout [] <|
            Element.column
                [ height fill
                , width fill
                ]
                [ el
                    [ Region.navigation
                    , width fill
                    ]
                    page.nav
                , column
                    [ width fill ]
                  <|
                    List.map
                        (\uiErr ->
                            paragraph
                                [ width fill
                                , Background.color colourError
                                , Font.color textColourLight
                                , padding size3
                                ]
                                [ text uiErr.failure.friendlyMessage ]
                        )
                    <|
                        List.Extra.uniqueBy (\uiErr -> uiErr.failure.friendlyMessage) model.errors
                , el
                    [ Region.mainContent
                    , width fill
                    , height fill
                    ]
                    page.body
                , el
                    [ Region.footer
                    , width fill
                    ]
                  <|
                    row []
                        [ text "Thorn" ]
                ]
        ]
    }


nav : List ( Msg, String ) -> Element Msg
nav navEntries =
    Element.row [] <|
        List.map
            (\( msg, label ) ->
                Input.button buttonStyles
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
                                [ Input.button buttonStyles
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
    column
        []
        [ Input.button buttonStyles
            { onPress = Just NavigateCreateGame
            , label = text "Create game"
            }
        , Input.button buttonStyles
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
        , Input.button buttonStyles
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
        , Input.button buttonStyles
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
                            Input.button buttonStyles
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


placeDisc : Model -> GameStatusMessage -> Maybe Disc -> Element Msg
placeDisc model gameStatus maybeDisc =
    let
        roseButton =
            if gameStatus.self.roseCount > 0 then
                Input.button buttonStyles
                    { onPress = Just <| InputPlaceDisc Rose
                    , label = text "Rose"
                    }

            else
                Element.none

        thornButton =
            if gameStatus.self.hasThorn then
                Input.button buttonStyles
                    { onPress = Just <| InputPlaceDisc Thorn
                    , label = text "Thorn"
                    }

            else
                Element.none

        buttons =
            case maybeDisc of
                Just disc ->
                    [ Input.button buttonStyles
                        { onPress = Just <| InputRemovePlaceDisc
                        , label = text "Clear"
                        }
                    , Input.button buttonStyles
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
        , row [] buttons
        , playersList gameStatus False
        ]


discOrBid : Model -> GameStatusMessage -> DiscOrBid -> Element Msg
discOrBid model gameStatus maybeSelection =
    let
        maxBid =
            numberOfPlacedDiscs gameStatus

        roseButton =
            if gameStatus.self.roseCount > 0 then
                Input.button buttonStyles
                    { onPress = Just <| InputPlaceDisc Rose
                    , label = text "Rose"
                    }

            else
                Element.none

        thornButton =
            if gameStatus.self.hasThorn then
                Input.button buttonStyles
                    { onPress = Just <| InputPlaceDisc Thorn
                    , label = text "Thorn"
                    }

            else
                Element.none

        bidButtons =
            List.map
                (\count ->
                    Input.button buttonStyles
                        { onPress = Just <| InputBid count
                        , label = text <| String.fromInt count
                        }
                )
            <|
                List.range (minBid gameStatus) maxBid

        bidButtonContainer =
            row [] bidButtons

        buttons =
            case maybeSelection of
                DiscOrBidDisc disc ->
                    [ Input.button buttonStyles
                        { onPress = Just InputRemovePlaceDisc
                        , label = text "Clear"
                        }
                    , Input.button buttonStyles
                        { onPress = Just <| SubmitPlaceDisc disc
                        , label = text "Submit"
                        }
                    ]

                DiscOrBidBid bid ->
                    [ Input.button buttonStyles
                        { onPress = Just InputRemoveBid
                        , label = text "Clear"
                        }
                    , Input.button buttonStyles
                        { onPress = Just <| SubmitBid bid
                        , label = text "Submit"
                        }
                    ]

                DiscOrBidNoSelection ->
                    [ roseButton, thornButton, bidButtonContainer ]
    in
    column []
        [ paragraph []
            [ text "Place disc or open the bidding" ]
        , row [] buttons
        , playersList gameStatus False
        ]


bidOrPass : Model -> GameStatusMessage -> BidOrPass -> Element Msg
bidOrPass model gameStatus maybeSelection =
    let
        maxBid =
            numberOfPlacedDiscs gameStatus

        bidButtons =
            List.map
                (\count ->
                    Input.button buttonStyles
                        { onPress = Just <| InputBid count
                        , label = text <| String.fromInt count
                        }
                )
            <|
                List.range (minBid gameStatus) maxBid

        bidButtonContainer =
            row [] bidButtons

        buttons =
            case maybeSelection of
                BidOrPassBid bid ->
                    [ Input.button buttonStyles
                        { onPress = Just InputRemoveBid
                        , label = text "Clear"
                        }
                    , Input.button buttonStyles
                        { onPress = Just <| SubmitBid bid
                        , label = text "Submit"
                        }
                    ]

                BidOrPassPass ->
                    [ Input.button buttonStyles
                        { onPress = Just InputRemovePass
                        , label = text "Clear"
                        }
                    , Input.button buttonStyles
                        { onPress = Just SubmitPass
                        , label = text "Submit"
                        }
                    ]

                BidOrPassNoSelection ->
                    [ bidButtonContainer
                    , Input.button buttonStyles
                        { onPress = Just InputPass
                        , label = text "Pass"
                        }
                    ]
    in
    column []
        [ paragraph []
            [ text "Bid or pass" ]
        , row [] buttons
        , playersList gameStatus False
        ]


flip : Model -> GameStatusMessage -> Maybe PlayerId -> Element Msg
flip model gameStatus maybeStack =
    let
        buttons =
            case maybeStack of
                Just stackId ->
                    [ Input.button buttonStyles
                        { onPress = Just InputRemoveFlip
                        , label = text "Clear"
                        }
                    , Input.button buttonStyles
                        { onPress = Just <| SubmitFlip stackId
                        , label = text "Submit"
                        }
                    ]

                Nothing ->
                    [ Element.none ]
    in
    column []
        [ paragraph []
            [ text "Flipping" ]
        , row [] buttons
        , playersList gameStatus True
        ]


currentGame : Model -> GameStatusMessage -> Element Msg
currentGame model gameStatus =
    let
        roundInfo =
            case gameStatus.game.round of
                Just (InitialDiscs initialDiscs) ->
                    paragraph []
                        [ text "Waiting for other players to place a disc" ]

                Just (Placing placing) ->
                    paragraph []
                        [ text "Waiting for other players to place a disc or start the bidding" ]

                Just (Bidding bidding) ->
                    paragraph []
                        [ text "Waiting for other players to bid or pass" ]

                Just (Flipping flipping) ->
                    paragraph []
                        [ text "Another player is trying to win the round" ]

                Just (Finished finished) ->
                    let
                        newRoundButton =
                            if isCreator gameStatus.game gameStatus.self then
                                Input.button buttonStyles
                                    { onPress = Just SubmitNewRound
                                    , label = text "Next round"
                                    }

                            else
                                Element.none

                        finishedMessage =
                            if finished.successful then
                                if finished.activePlayer == gameStatus.self.playerId then
                                    paragraph []
                                        [ text "You have won the round" ]

                                else
                                    paragraph []
                                        [ text "Another player has won the round" ]

                            else if finished.activePlayer == gameStatus.self.playerId then
                                paragraph []
                                    [ text "You have hit a skull and failed to win the round" ]

                            else
                                paragraph []
                                    [ text "Another player failed to win the round" ]
                    in
                    column []
                        [ finishedMessage, newRoundButton ]

                Nothing ->
                    Element.none
    in
    column
        []
        [ roundInfo
        , text <|
            if gameStatus.self.hasThorn then
                "Thorn"

            else
                "No Thorn"
        , text <|
            if gameStatus.self.roseCount == 0 then
                "No Roses"

            else if gameStatus.self.roseCount == 1 then
                "1 Rose"

            else
                String.fromInt gameStatus.self.roseCount ++ " Roses"
        , playersList gameStatus False
        ]


playersList : GameStatusMessage -> Bool -> Element Msg
playersList gameStatus showStackSelector =
    let
        otherPlayers =
            List.filter
                (\player -> player.playerId /= gameStatus.self.playerId)
            <|
                reorderToBy .playerId gameStatus.self.playerId gameStatus.game.players
    in
    column []
        [ column []
            [ text gameStatus.self.screenName
            , text <|
                if gameStatus.self.hasThorn then
                    "Thorn"

                else
                    "No Thorn"
            , text <|
                if gameStatus.self.roseCount == 0 then
                    "No Roses"

                else if gameStatus.self.roseCount == 1 then
                    "1 Rose"

                else
                    String.fromInt gameStatus.self.roseCount ++ " Roses"
            ]
        , column [] <|
            List.map (playerDisplay gameStatus showStackSelector) otherPlayers
        ]


playerDisplay : GameStatusMessage -> Bool -> Player -> Element Msg
playerDisplay gameStatus showStackSelector player =
    row [ spacing size3 ]
        [ text player.screenName
        , text <| String.fromInt player.discCount
        , if showStackSelector then
            Input.button buttonStyles
                { onPress = Just <| InputFlip player.playerId
                , label = text "Flip"
                }

          else
            Element.none
        ]
