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
import GameLogic exposing (allFlipped, gameWinner, hasPlacedThorn, isCreator, minBid, numberOfPlacedDiscs, placedRoseCount, playerIsActive, selfIsActive)
import Html.Attributes
import List.Extra
import Maybe.Extra
import Model exposing (..)
import Utils exposing (reorderToBy, swapDown, swapUp)
import Views.Styles exposing (buttonStyles, colourAlt, colourError, colourHighlight, colourPrimary, colourSecondary, colourSecondary2, colourSecondaryLight, colourWhite, size1, size2, size3, size4, textColourFeature, textColourLight)


type alias Page =
    { title : String
    , body : Element Msg
    , nav : Element Msg
    , loading : LoadingStatus
    , ui : String
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
                    , loading = NotLoading
                    , ui = "welcome"
                    }

                CreateGameScreen gameName screenName loadingStatus ->
                    { title = "Create game | Thorn"
                    , body = createGame model gameName screenName loadingStatus
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    , loading = loadingStatus
                    , ui = "create-game"
                    }

                JoinGameScreen gameCode screenName loadingStatus ->
                    { title = "Join game | Thorn"
                    , body = joinGame model gameCode screenName loadingStatus
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    , loading = loadingStatus
                    , ui = "join-game"
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
                    , loading = loadingStatus
                    , ui = "lobby"
                    }

                DisplayGameScreen gameStatus _ ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , body = currentGame model gameStatus
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    , loading = NotLoading
                    , ui = "display-game"
                    }

                PlaceDiscScreen maybeDisc gameStatus _ loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , body = placeDisc model gameStatus maybeDisc
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    , loading = loadingStatus
                    , ui = "place-disc"
                    }

                DiscOrBidScreen maybeSelection gameStatus _ loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , body = discOrBid model gameStatus maybeSelection
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    , loading = loadingStatus
                    , ui = "disc-or-bid"
                    }

                BidOrPassScreen maybeInt gameStatus _ loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , body = bidOrPass model gameStatus maybeInt
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    , loading = loadingStatus
                    , ui = "bid-or-pass"
                    }

                FlipScreen maybeStack gameStatus _ loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , body = flip model gameStatus maybeStack
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    , loading = loadingStatus
                    , ui = "flip"
                    }
    in
    { title = page.title
    , body =
        [ layout
            [ Font.family
                [ Font.typeface "Nunito"
                , Font.sansSerif
                ]
            ]
          <|
            Element.column
                [ height fill
                , width fill
                , Element.htmlAttribute <| Html.Attributes.class <| "ui--" ++ page.ui
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
                , if page.loading == AwaitingMessage then
                    text "loading..."

                  else
                    Element.none
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
        -- TODO: only show reoder controls to creator
        playersEl =
            if List.isEmpty playerOrder then
                text "Waiting for other players"

            else
                column
                    [ width fill ]
                <|
                    List.map
                        (\player ->
                            let
                                last =
                                    Maybe.withDefault False <|
                                        Maybe.map
                                            (\p -> p == player)
                                        <|
                                            List.Extra.last playerOrder

                                first =
                                    Maybe.withDefault False <|
                                        Maybe.map
                                            (\p -> p == player)
                                        <|
                                            List.head playerOrder
                            in
                            el
                                [ width fill
                                , Border.widthEach
                                    { bottom = 0
                                    , left = 0
                                    , right = 0
                                    , top = size1
                                    }
                                , Border.color colourSecondaryLight
                                ]
                            <|
                                row
                                    [ width fill
                                    , padding size4
                                    , Border.widthEach
                                        { bottom = size1
                                        , left = size4
                                        , right = 0
                                        , top = 0
                                        }
                                    , Border.color colourPrimary
                                    , Background.gradient
                                        { angle = 0
                                        , steps = [ colourSecondary, colourSecondary2 ]
                                        }
                                    ]
                                    [ text player.screenName
                                    , if first then
                                        Element.none

                                      else
                                        Input.button buttonStyles
                                            { onPress = Just <| InputReorderPlayers <| swapUp player playerOrder
                                            , label = text "Up"
                                            }
                                    , if last then
                                        Element.none

                                      else
                                        Input.button buttonStyles
                                            { onPress = Just <| InputReorderPlayers <| swapDown player playerOrder
                                            , label = text "Down"
                                            }
                                    ]
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
        [ width fill ]
        [ text "Lobby"
        , text "Game code"
        , el
            [ uiHook "game-code" ]
          <|
            text <|
                gameCode welcomeMessage.gameId
        , playersEl
        , startGameEl
        ]


placeDisc : Model -> GameStatusMessage -> Maybe Disc -> Element Msg
placeDisc model gameStatus maybeDisc =
    let
        roseButton =
            if gameStatus.self.roseCount > placedRoseCount gameStatus then
                Input.button buttonStyles
                    { onPress = Just <| InputPlaceDisc Rose
                    , label = text "Rose"
                    }

            else
                Element.none

        thornButton =
            if gameStatus.self.hasThorn && (not <| hasPlacedThorn gameStatus) then
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
    column
        [ width fill ]
        [ selfSecretInformation gameStatus
        , paragraph []
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
            if gameStatus.self.roseCount > placedRoseCount gameStatus then
                Input.button buttonStyles
                    { onPress = Just <| InputPlaceDisc Rose
                    , label = text "Rose"
                    }

            else
                Element.none

        thornButton =
            if gameStatus.self.hasThorn && (not <| hasPlacedThorn gameStatus) then
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
    column
        [ width fill ]
        [ selfSecretInformation gameStatus
        , paragraph []
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
    column
        [ width fill ]
        [ selfSecretInformation gameStatus
        , paragraph []
            [ text "Bid or pass" ]
        , row [] buttons
        , playersList gameStatus False
        ]


flip : Model -> GameStatusMessage -> Maybe PlayerId -> Element Msg
flip model gameStatus maybeStack =
    let
        allOwnFlipped =
            allFlipped gameStatus gameStatus.self.playerId

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
                    if allOwnFlipped then
                        [ Element.none ]

                    else
                        [ Input.button buttonStyles
                            { onPress = Just <| InputFlip gameStatus.self.playerId
                            , label = text "Flip"
                            }
                        ]
    in
    column
        [ width fill ]
        [ selfSecretInformation gameStatus
        , paragraph []
            [ text "Flipping" ]
        , row [] buttons
        , playersList gameStatus allOwnFlipped
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
                        maybeWinner =
                            gameWinner gameStatus

                        winnerInfo =
                            case maybeWinner of
                                Just winner ->
                                    if winner.playerId == gameStatus.self.playerId then
                                        paragraph []
                                            [ text "You have won the game, congratulations!" ]

                                    else
                                        paragraph []
                                            [ text <| winner.screenName ++ " has won!" ]

                                Nothing ->
                                    Element.none

                        newRoundButton =
                            if Maybe.Extra.isNothing maybeWinner && isCreator gameStatus.game gameStatus.self then
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
                        [ winnerInfo, finishedMessage, newRoundButton ]

                Nothing ->
                    Element.none
    in
    column
        [ width fill ]
        [ selfSecretInformation gameStatus
        , roundInfo
        , playersList gameStatus False
        ]


selfSecretInformation : GameStatusMessage -> Element Msg
selfSecretInformation gameStatus =
    let
        placed =
            Maybe.withDefault [] gameStatus.self.placedDiscs

        thornEl =
            if gameStatus.self.hasThorn then
                discDisplay Thorn

            else
                Element.none

        roseEls =
            List.map discDisplay <|
                List.repeat gameStatus.self.roseCount Rose

        poolEls =
            List.reverse <| thornEl :: roseEls
    in
    column
        [ width fill ]
        [ column
            [ width fill ]
            [ text "pool"
            , row
                [ spacing size3 ]
                poolEls
            ]
        , column
            [ width fill ]
            [ text "placed"
            , row
                [ spacing size3 ]
              <|
                List.map discDisplay placed
            ]
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
    column
        [ width fill ]
    <|
        List.map (playerPublicInformation gameStatus showStackSelector) otherPlayers


playerPublicInformation : GameStatusMessage -> Bool -> Player -> Element Msg
playerPublicInformation gameStatus showStackSelector player =
    let
        pid =
            getPid player.playerId

        active =
            playerIsActive gameStatus player

        revealedDiscs =
            case gameStatus.game.round of
                Just (InitialDiscs _) ->
                    []

                Just (Placing _) ->
                    []

                Just (Bidding _) ->
                    []

                Just (Flipping flippingData) ->
                    Maybe.withDefault [] <|
                        Dict.get pid flippingData.revealed

                Just (Finished finishedData) ->
                    Maybe.withDefault [] <|
                        Dict.get pid finishedData.revealed

                Nothing ->
                    []

        placedDiscCount =
            Maybe.withDefault 0 <|
                case gameStatus.game.round of
                    Just (InitialDiscs initialDiscsData) ->
                        Dict.get pid initialDiscsData.initialDiscs

                    Just (Placing placingData) ->
                        Dict.get pid placingData.discs

                    Just (Bidding biddingData) ->
                        Dict.get pid biddingData.discs

                    Just (Flipping flippingData) ->
                        Dict.get pid flippingData.discs

                    Just (Finished finishedData) ->
                        Dict.get pid finishedData.discs

                    Nothing ->
                        Just 0

        unrevealedDiscCount =
            placedDiscCount - List.length revealedDiscs

        hasDiscsUnflipped =
            not <| allFlipped gameStatus player.playerId

        info =
            row
                [ width fill ]
                [ el
                    [ Font.color textColourLight ]
                  <|
                    text player.screenName
                , if hasDiscsUnflipped && showStackSelector then
                    Input.button buttonStyles
                        { onPress = Just <| InputFlip player.playerId
                        , label = text "Flip"
                        }

                  else
                    Element.none
                ]
    in
    el
        [ width fill
        , Border.widthEach
            { bottom = 0
            , left = 0
            , right = 0
            , top = size1
            }
        , Border.color colourSecondaryLight
        ]
    <|
        el
            [ width fill
            , Border.widthEach
                { bottom = 0
                , left = size4
                , right = 0
                , top = 0
                }
            , Border.color <|
                if active then
                    colourHighlight

                else
                    colourPrimary
            ]
        <|
            column
                [ width fill
                , padding size4
                , Border.widthEach
                    { bottom = size1
                    , left = 0
                    , right = 0
                    , top = 0
                    }
                , Border.color colourPrimary
                , Background.gradient
                    { angle = 0
                    , steps = [ colourSecondary, colourSecondary2 ]
                    }
                ]
                [ info
                , row
                    [ width fill ]
                  <|
                    List.repeat player.discCount unknownDiscDisplay
                , row
                    [ width fill ]
                  <|
                    List.append
                        (List.map discDisplay revealedDiscs)
                        (List.repeat unrevealedDiscCount unknownDiscDisplay)
                ]


discDisplay : Disc -> Element Msg
discDisplay disc =
    let
        discTypeEl =
            case disc of
                Thorn ->
                    text "T"

                Rose ->
                    text "R"
    in
    el
        [ width <| px 40
        , height <| px 40
        , Border.rounded 20
        , Background.color colourAlt
        , Border.solid
        , Border.color textColourFeature
        , Font.color textColourLight
        , centerY
        ]
    <|
        el [ centerY, centerX ] <|
            discTypeEl


unknownDiscDisplay : Element Msg
unknownDiscDisplay =
    el
        [ width <| px 40
        , height <| px 40
        , Border.rounded 20
        , Background.color colourAlt
        , Border.solid
        , Border.color textColourFeature
        , Font.color textColourLight
        , centerY
        ]
    <|
        el [ centerY, centerX ] <|
            text "?"


uiHook : String -> Attribute Msg
uiHook name =
    -- Allows automated testing to target the element
    Element.htmlAttribute <| Html.Attributes.class <| "ui-hook--" ++ name
