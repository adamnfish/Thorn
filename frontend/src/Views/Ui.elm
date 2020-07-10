module Views.Ui exposing (view)

import Browser
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import FontAwesome.Attributes as Icon
import FontAwesome.Icon as Icon exposing (Icon)
import FontAwesome.Solid as Icon
import FontAwesome.Styles
import GameLogic exposing (allFlipped, gameWinner, hasPlacedThorn, isCreator, minBid, numberOfPlacedDiscs, placedRoseCount, playerIsActive, selfAsPlayer, selfIsActive)
import Html.Attributes
import List.Extra
import Maybe.Extra
import Model exposing (..)
import Utils exposing (reorderToBy, swapDown, swapUp)
import Views.Styles exposing (buttonStyles, centerBlock, colourAlt, colourAltSecondary, colourBlack, colourBlack2, colourCta, colourError, colourHighlight, colourHighlight2, colourPrimary, colourSecondary, colourSecondary2, colourSecondaryHighlight, colourSecondaryLight, colourWhite, each0, featureButtonStyles, fontSizeSmall, formatColor, length4, size1, size2, size3, size4, size5, size6, spacer, textColourDark, textColourFeature, textColourGrey, textColourLight, textInputStyles)


type alias Page =
    { title : String
    , status : String
    , body : Element Msg
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
                    , status = "Welcome"
                    , body = home model
                    , loading = NotLoading
                    , ui = "welcome"
                    }

                CreateGameScreen gameName screenName loadingStatus ->
                    { title = "Create game | Thorn"
                    , status = "Create game"
                    , body = createGame model gameName screenName loadingStatus
                    , loading = loadingStatus
                    , ui = "create-game"
                    }

                JoinGameScreen gameCode screenName loadingStatus ->
                    { title = "Join game | Thorn"
                    , status = "Join game"
                    , body = joinGame model gameCode screenName loadingStatus
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
                    , status = "Waiting for game to start"
                    , body = lobby model playerOrder loadingStatus welcomeMessage maybeGameStatus
                    , loading = loadingStatus
                    , ui = "lobby"
                    }

                DisplayGameScreen gameStatus _ ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , status = "Waiting"
                    , body = currentGame model gameStatus
                    , loading = NotLoading
                    , ui = "display-game"
                    }

                PlaceDiscScreen maybeDisc gameStatus _ loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , status = "Place disc"
                    , body = placeDisc model gameStatus maybeDisc
                    , loading = loadingStatus
                    , ui = "place-disc"
                    }

                DiscOrBidScreen maybeSelection gameStatus _ loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , status = "Place disc or bid"
                    , body = discOrBid model gameStatus maybeSelection
                    , loading = loadingStatus
                    , ui = "disc-or-bid"
                    }

                BidOrPassScreen maybeInt gameStatus _ loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , status = "Bid or pass"
                    , body = bidOrPass model gameStatus maybeInt
                    , loading = loadingStatus
                    , ui = "bid-or-pass"
                    }

                FlipScreen maybeStack gameStatus _ loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , status = "Choose disc to flip"
                    , body = flip model gameStatus maybeStack
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
            , Background.color <| rgb255 191 189 193
            ]
          <|
            Element.column
                [ height fill
                , width fill
                , Element.htmlAttribute <| Html.Attributes.class <| "ui--" ++ page.ui
                ]
                [ Element.html FontAwesome.Styles.css
                , row
                    [ width fill
                    , padding size4
                    , Background.gradient
                        { angle = 0
                        , steps = [ colourHighlight, colourHighlight2 ]
                        }
                    ]
                    [ Input.button []
                        { onPress = Just NavigateHome
                        , label = text "Thorn"
                        }
                    ]
                , row
                    [ width fill
                    , padding size4
                    , spacing size4
                    , Background.gradient
                        { angle = 0
                        , steps = [ colourBlack, colourBlack2 ]
                        }
                    , Font.color textColourLight
                    , fontSizeSmall
                    ]
                    [ text page.status
                    , el
                        [ alignRight ]
                      <|
                        case page.loading of
                            AwaitingMessage ->
                                row
                                    [ spacing size3 ]
                                    [ text "loading"
                                    , text ""
                                    , Element.html
                                        (Icon.circleNotch
                                            |> Icon.present
                                            |> Icon.styled [ Icon.spin ]
                                            |> Icon.view
                                        )
                                    ]

                            NotLoading ->
                                text ""
                    , el [] <| text ""
                    , el
                        [ alignRight ]
                      <|
                        if model.connected then
                            Element.html
                                (Icon.link
                                    |> Icon.present
                                    |> Icon.view
                                )

                        else
                            Element.html
                                (Icon.unlink
                                    |> Icon.present
                                    |> Icon.view
                                )
                    ]
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
                , spacer 1
                , el
                    [ Region.mainContent
                    , width fill
                    , height fill
                    ]
                    page.body
                , el
                    [ width fill
                    , height length4
                    ]
                    Element.none
                , row
                    [ Region.footer
                    , width fill
                    , height <| px 60
                    , padding size4
                    , Border.widthEach
                        { each0 | top = size2 }
                    , Border.color colourHighlight
                    , Background.gradient
                        { angle = 0
                        , steps = [ colourBlack, colourBlack2 ]
                        }
                    , Font.color textColourLight
                    ]
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
    centerBlock <|
        column
            [ width fill
            , padding size4
            , spacing size4
            ]
            [ Input.button featureButtonStyles
                { onPress = Just NavigateCreateGame
                , label = text "Create game"
                }
            , Input.button featureButtonStyles
                { onPress = Just NavigateJoinGame
                , label = text "Join game"
                }
            , if List.isEmpty availableGames then
                Element.none

              else
                column
                    [ width fill
                    ]
                    [ spacer 2
                    , text "Rejoin existing game"
                    , column [] availableGames
                    ]
            ]


createGame : Model -> String -> String -> LoadingStatus -> Element Msg
createGame model gameName screenName loadingStatus =
    centerBlock <|
        column
            [ width fill
            , padding size4
            , spacing size4
            ]
            [ Input.text textInputStyles
                { onChange = \newGameName -> InputCreateGame newGameName screenName loadingStatus
                , text = gameName
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Game name"
                }
            , Input.text textInputStyles
                { onChange = \newScreenName -> InputCreateGame gameName newScreenName loadingStatus
                , text = screenName
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Screen name"
                }
            , spacer 2
            , Input.button featureButtonStyles
                { onPress = Just <| SubmitCreateGame gameName screenName
                , label = text "Create game"
                }
            ]


joinGame : Model -> String -> String -> LoadingStatus -> Element Msg
joinGame model gameCode screenName loadingStatus =
    centerBlock <|
        column
            [ width fill
            , padding size4
            , spacing size4
            ]
            [ Input.text textInputStyles
                { onChange = \newGameName -> InputJoinGame newGameName screenName loadingStatus
                , text = gameCode
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Game code"
                }
            , Input.text textInputStyles
                { onChange = \newScreenName -> InputJoinGame gameCode newScreenName loadingStatus
                , text = screenName
                , placeholder = Nothing
                , label = Input.labelAbove [] <| text "Screen name"
                }
            , spacer 2
            , Input.button featureButtonStyles
                { onPress = Just <| SubmitJoinGame gameCode screenName
                , label = text "Join game"
                }
            ]


lobby : Model -> List Player -> LoadingStatus -> WelcomeMessage -> Maybe GameStatusMessage -> Element Msg
lobby model playerOrder loadingStatus welcomeMessage maybeGameStatus =
    let
        showReorderControls =
            case maybeGameStatus of
                Just gameStatus ->
                    isCreator gameStatus.game gameStatus.self

                _ ->
                    False

        playerEls =
            List.map (lobbyPlayersList showReorderControls playerOrder) playerOrder

        missingPlayerEl =
            el
                [ width fill
                , Border.widthEach
                    { each0 | top = size1 }
                , Border.color colourSecondaryHighlight
                ]
            <|
                el
                    [ width fill
                    , Border.widthEach
                        { each0 | left = size4 }
                    , Border.color colourCta
                    ]
                <|
                    row
                        [ width fill
                        , height <| px 55
                        , padding size4
                        , spacing size4
                        , Border.widthEach
                            { each0 | bottom = size1 }
                        , Border.color colourBlack
                        , Background.gradient
                            { angle = 0
                            , steps = [ colourSecondary, colourSecondary2 ]
                            }
                        , Font.color textColourLight
                        ]
                        [ Element.html
                            (Icon.userPlus
                                |> Icon.present
                                |> Icon.styled [ Html.Attributes.style "color" <| formatColor textColourGrey ]
                                |> Icon.view
                            )
                        ]

        missingPlayerEls =
            List.repeat
                (max 0 <| 3 - List.length playerOrder)
                missingPlayerEl

        additionalPlayerEl =
            el
                [ width fill
                , Border.widthEach
                    { each0 | top = size1 }
                , Border.color colourSecondaryHighlight
                , alpha 0.5
                ]
            <|
                el
                    [ width fill
                    , Border.widthEach
                        { each0 | left = size4 }
                    , Border.color colourWhite
                    ]
                <|
                    row
                        [ width fill
                        , height <| px 55
                        , padding size4
                        , spacing size4
                        , Border.widthEach
                            { each0 | bottom = size1 }
                        , Border.color colourBlack
                        , Background.gradient
                            { angle = 0
                            , steps = [ colourSecondary, colourSecondary2 ]
                            }
                        , Font.color textColourLight
                        ]
                        [ Element.html
                            (Icon.userPlus
                                |> Icon.present
                                |> Icon.styled [ Html.Attributes.style "color" "rgba(180, 180, 180, 0.8)" ]
                                |> Icon.view
                            )
                        ]

        startGameEl =
            case maybeGameStatus of
                Just gameStatus ->
                    if isCreator gameStatus.game gameStatus.self then
                        if List.length playerOrder >= 3 then
                            column
                                [ width fill ]
                                [ spacer 1
                                , Input.button featureButtonStyles
                                    { onPress = Just SubmitStartGame
                                    , label = text "Start game"
                                    }
                                ]

                        else
                            statusMessage "There must be at least 3 players to start the game"

                    else
                        Element.none

                Nothing ->
                    Element.none
    in
    centerBlock <|
        column
            [ width fill
            , spacing size3
            ]
            [ row
                [ width fill
                , spacing size6
                ]
                [ el
                    [ uiHook "game-code"
                    , padding size5
                    , alignTop
                    , Font.bold
                    , Font.size 45
                    , Font.color textColourDark
                    , Font.alignLeft
                    , Background.color colourWhite
                    , Border.solid
                    , Border.widthEach
                        { each0 | bottom = size1 }
                    , Border.color colourSecondary
                    , Border.rounded 0
                    ]
                  <|
                    text <|
                        gameCode welcomeMessage.gameId
                , paragraph
                    [ width fill
                    , Font.alignLeft
                    ]
                    [ text "Other players can use this code to join your game."
                    ]
                ]
            , spacer 2
            , column
                [ width fill ]
              <|
                List.append
                    (List.append playerEls missingPlayerEls)
                    (List.singleton additionalPlayerEl)
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
                        , label =
                            row
                                [ spacing size4 ]
                                [ el
                                    []
                                  <|
                                    text "Clear"
                                , text ""
                                , Element.html
                                    (Icon.times
                                        |> Icon.present
                                        |> Icon.view
                                    )
                                ]
                        }
                    , Input.button buttonStyles
                        { onPress = Just <| SubmitPlaceDisc disc
                        , label =
                            row
                                [ spacing size4 ]
                                [ el
                                    []
                                  <|
                                    text "Confirm"
                                , text ""
                                , Element.html
                                    (Icon.check
                                        |> Icon.present
                                        |> Icon.view
                                    )
                                ]
                        }
                    ]

                Nothing ->
                    [ roseButton, thornButton ]
    in
    centerBlock <|
        column
            [ width fill ]
            [ selfSecretInformation gameStatus
            , spacer 3
            , playersList gameStatus False
            , row
                [ spacing size2 ]
                buttons
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
            row
                [ spacing size2 ]
                bidButtons

        buttons =
            case maybeSelection of
                DiscOrBidDisc disc ->
                    [ Input.button buttonStyles
                        { onPress = Just InputRemovePlaceDisc
                        , label =
                            row
                                [ spacing size4 ]
                                [ el
                                    []
                                  <|
                                    text "Clear"
                                , text ""
                                , Element.html
                                    (Icon.times
                                        |> Icon.present
                                        |> Icon.view
                                    )
                                ]
                        }
                    , Input.button buttonStyles
                        { onPress = Just <| SubmitPlaceDisc disc
                        , label =
                            row
                                [ spacing size4 ]
                                [ el
                                    []
                                  <|
                                    text "Confirm"
                                , text ""
                                , Element.html
                                    (Icon.check
                                        |> Icon.present
                                        |> Icon.view
                                    )
                                ]
                        }
                    ]

                DiscOrBidBid bid ->
                    [ Input.button buttonStyles
                        { onPress = Just InputRemoveBid
                        , label =
                            row
                                [ spacing size4 ]
                                [ el
                                    []
                                  <|
                                    text "Clear"
                                , text ""
                                , Element.html
                                    (Icon.times
                                        |> Icon.present
                                        |> Icon.view
                                    )
                                ]
                        }
                    , Input.button buttonStyles
                        { onPress = Just <| SubmitBid bid
                        , label =
                            row
                                [ spacing size4 ]
                                [ el
                                    []
                                  <|
                                    text "Confirm"
                                , text ""
                                , Element.html
                                    (Icon.check
                                        |> Icon.present
                                        |> Icon.view
                                    )
                                ]
                        }
                    ]

                DiscOrBidNoSelection ->
                    [ roseButton, thornButton, bidButtonContainer ]
    in
    centerBlock <|
        column
            [ width fill ]
            [ selfSecretInformation gameStatus
            , spacer 3
            , playersList gameStatus False
            , row
                [ spacing size2 ]
                buttons
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
                        , label =
                            row
                                [ spacing size4 ]
                                [ el
                                    []
                                  <|
                                    text "Clear"
                                , text ""
                                , Element.html
                                    (Icon.times
                                        |> Icon.present
                                        |> Icon.view
                                    )
                                ]
                        }
                    , Input.button buttonStyles
                        { onPress = Just <| SubmitBid bid
                        , label =
                            row
                                [ spacing size4 ]
                                [ el
                                    []
                                  <|
                                    text "Confirm"
                                , text ""
                                , Element.html
                                    (Icon.check
                                        |> Icon.present
                                        |> Icon.view
                                    )
                                ]
                        }
                    ]

                BidOrPassPass ->
                    [ Input.button buttonStyles
                        { onPress = Just InputRemovePass
                        , label =
                            row
                                [ spacing size4 ]
                                [ el
                                    []
                                  <|
                                    text "Clear"
                                , text ""
                                , Element.html
                                    (Icon.times
                                        |> Icon.present
                                        |> Icon.view
                                    )
                                ]
                        }
                    , Input.button buttonStyles
                        { onPress = Just SubmitPass
                        , label =
                            row
                                [ spacing size4 ]
                                [ el
                                    []
                                  <|
                                    text "Confirm"
                                , text ""
                                , Element.html
                                    (Icon.check
                                        |> Icon.present
                                        |> Icon.view
                                    )
                                ]
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
    centerBlock <|
        column
            [ width fill ]
            [ selfSecretInformation gameStatus
            , spacer 3
            , playersList gameStatus False
            , row
                [ spacing size2 ]
                buttons
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
                        , label =
                            row
                                [ spacing size4 ]
                                [ el
                                    []
                                  <|
                                    text "Clear"
                                , text ""
                                , Element.html
                                    (Icon.times
                                        |> Icon.present
                                        |> Icon.view
                                    )
                                ]
                        }
                    , Input.button buttonStyles
                        { onPress = Just <| SubmitFlip stackId
                        , label =
                            row
                                [ spacing size4 ]
                                [ el
                                    []
                                  <|
                                    text "Confirm"
                                , text ""
                                , Element.html
                                    (Icon.check
                                        |> Icon.present
                                        |> Icon.view
                                    )
                                ]
                        }
                    ]

                Nothing ->
                    if allOwnFlipped then
                        [ Element.none ]

                    else
                        [ Input.button buttonStyles
                            { onPress = Just <| InputFlip gameStatus.self.playerId
                            , label =
                                row
                                    [ spacing size4 ]
                                    [ el
                                        []
                                      <|
                                        text "Flip"
                                    , text ""
                                    , Element.html
                                        (Icon.sync
                                            |> Icon.present
                                            |> Icon.view
                                        )
                                    ]
                            }
                        ]
    in
    centerBlock <|
        column
            [ width fill ]
            [ selfSecretInformation gameStatus
            , spacer 3
            , playersList gameStatus allOwnFlipped
            , row
                [ spacing size2 ]
                buttons
            ]


currentGame : Model -> GameStatusMessage -> Element Msg
currentGame model gameStatus =
    let
        roundInfo =
            case gameStatus.game.round of
                Just (InitialDiscs initialDiscs) ->
                    statusMessage "Waiting for other players"

                Just (Placing placing) ->
                    statusMessage "Waiting for other players"

                Just (Bidding bidding) ->
                    statusMessage "Waiting for other players"

                Just (Flipping flipping) ->
                    statusMessage "Another player is flipping"

                Just (Finished finished) ->
                    let
                        maybeWinner =
                            gameWinner gameStatus

                        winnerInfo =
                            case maybeWinner of
                                Just winner ->
                                    if winner.playerId == gameStatus.self.playerId then
                                        statusMessage "You have won the game, congratulations!"

                                    else
                                        statusMessage <| winner.screenName ++ " has won!"

                                Nothing ->
                                    Element.none

                        newRoundButton =
                            if isCreator gameStatus.game gameStatus.self && Maybe.Extra.isNothing maybeWinner then
                                Input.button buttonStyles
                                    { onPress = Just SubmitNewRound
                                    , label = text "Next round"
                                    }

                            else
                                Element.none

                        finishedMessage =
                            if Maybe.Extra.isNothing maybeWinner then
                                if finished.successful then
                                    if finished.activePlayer == gameStatus.self.playerId then
                                        statusMessage "You have won the round"

                                    else
                                        statusMessage "Another player has won the round"

                                else if finished.activePlayer == gameStatus.self.playerId then
                                    statusMessage "You have hit a skull and failed to win the round"

                                else
                                    statusMessage "Another player failed to win the round"

                            else
                                Element.none
                    in
                    column
                        [ width fill ]
                        [ winnerInfo, finishedMessage, newRoundButton ]

                Nothing ->
                    Element.none
    in
    centerBlock <|
        column
            [ width fill ]
            [ selfSecretInformation gameStatus
            , spacer 3
            , playersList gameStatus False
            , roundInfo
            ]


selfSecretInformation : GameStatusMessage -> Element Msg
selfSecretInformation gameStatus =
    let
        placed =
            Maybe.withDefault [] gameStatus.self.placedDiscs

        placedRoseCount =
            List.Extra.count (\d -> d == Rose) placed

        thornPoolEl =
            if gameStatus.self.hasThorn && not (List.member Thorn placed) then
                discDisplay Thorn

            else
                Element.none

        rosePoolEls =
            List.map discDisplay <|
                List.repeat (gameStatus.self.roseCount - placedRoseCount) Rose

        poolEls =
            List.reverse <| thornPoolEl :: rosePoolEls
    in
    el
        [ width fill
        , Background.color colourWhite
        ]
    <|
        el
            [ width fill
            , Border.widthEach
                { each0 | left = size4 }
            , Border.color colourAlt
            ]
        <|
            column
                [ width fill ]
                [ row
                    [ width fill
                    , spacing size4
                    , padding size4
                    ]
                    [ row
                        [ spacing size4
                        , paddingEach { each0 | right = size4 }
                        , Border.solid
                        , Border.widthEach { each0 | right = 1 }
                        , Border.color textColourGrey
                        ]
                        poolEls
                    , row
                        [ spacing size4
                        , alignRight
                        ]
                      <|
                        List.map discDisplay placed
                    ]
                , row
                    [ width fill
                    , padding size4
                    , fontSizeSmall
                    ]
                    [ text "Keep this secret"
                    , el
                        [ alignRight
                        , paddingXY size5 0
                        ]
                      <|
                        Element.html
                            (Icon.eye
                                |> Icon.present
                                |> Icon.view
                            )
                    ]
                ]


lobbyPlayersList : Bool -> List Player -> Player -> Element Msg
lobbyPlayersList showReorderControls playerOrder player =
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
            { each0 | top = size1 }
        , Border.color colourSecondaryHighlight
        ]
    <|
        row
            [ width fill
            , height <| px 55
            , padding size4
            , spacing size4
            , Border.widthEach
                { each0
                    | bottom = size1
                    , left = size4
                }
            , Border.color colourBlack
            , Background.gradient
                { angle = 0
                , steps = [ colourSecondary, colourSecondary2 ]
                }
            , Font.color textColourLight
            ]
            [ Element.html
                (Icon.userCircle
                    |> Icon.present
                    |> Icon.styled [ Html.Attributes.style "color" <| formatColor textColourLight ]
                    |> Icon.view
                )
            , el
                []
              <|
                text ""
            , text player.screenName
            , if showReorderControls then
                row
                    [ spacing size4
                    , width fill
                    , alignRight
                    ]
                    [ if first then
                        Element.none

                      else
                        el
                            [ alignRight ]
                        <|
                            Input.button buttonStyles
                                { onPress = Just <| InputReorderPlayers <| swapUp player playerOrder
                                , label =
                                    row
                                        [ spacing size4 ]
                                        [ Element.html
                                            (Icon.chevronUp
                                                |> Icon.present
                                                |> Icon.view
                                            )
                                        ]
                                }
                    , if last then
                        Element.none

                      else
                        el
                            [ alignRight ]
                        <|
                            Input.button buttonStyles
                                { onPress = Just <| InputReorderPlayers <| swapDown player playerOrder
                                , label =
                                    row
                                        [ spacing size4 ]
                                        [ Element.html
                                            (Icon.chevronDown
                                                |> Icon.present
                                                |> Icon.view
                                            )
                                        ]
                                }
                    ]

              else
                Element.none
            ]


playersList : GameStatusMessage -> Bool -> Element Msg
playersList gameStatus showStackSelector =
    let
        otherPlayers =
            List.filter
                (\player -> player.playerId /= gameStatus.self.playerId)
            <|
                reorderToBy .playerId gameStatus.self.playerId gameStatus.game.players

        otherPlayerEls =
            List.map (playerPublicInformation gameStatus showStackSelector) otherPlayers

        selfPlayer =
            selfAsPlayer gameStatus.self

        selfEl =
            playerPublicInformation gameStatus showStackSelector selfPlayer
    in
    column
        [ width fill ]
    <|
        selfEl
            :: otherPlayerEls


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
                [ width fill
                , spacing size4
                , height <| px 35
                ]
                [ Element.html
                    (Icon.userCircle
                        |> Icon.present
                        |> Icon.styled [ Html.Attributes.style "color" <| formatColor textColourLight ]
                        |> Icon.view
                    )
                , el [] <| text ""
                , el
                    [ Font.color textColourLight ]
                  <|
                    text player.screenName
                , if hasDiscsUnflipped && showStackSelector then
                    el
                        [ alignRight ]
                    <|
                        Input.button buttonStyles
                            { onPress = Just <| InputFlip player.playerId
                            , label =
                                row
                                    [ spacing size4 ]
                                    [ el
                                        []
                                      <|
                                        text "Flip"
                                    , text ""
                                    , Element.html
                                        (Icon.sync
                                            |> Icon.present
                                            |> Icon.view
                                        )
                                    ]
                            }

                  else
                    Element.none
                ]
    in
    el
        [ width fill
        , Border.widthEach
            { each0 | top = size1 }
        , Border.color colourSecondaryHighlight
        ]
    <|
        el
            [ width fill
            , Border.widthEach
                { each0 | left = size4 }
            , Border.color <|
                if active then
                    colourHighlight

                else
                    colourBlack
            ]
        <|
            column
                [ width fill
                , padding size4
                , Border.widthEach
                    { each0 | bottom = size1 }
                , Border.color colourBlack
                , Background.gradient
                    { angle = 0
                    , steps = [ colourSecondary, colourSecondary2 ]
                    }
                , spacing size4
                ]
                [ info
                , row
                    [ width fill ]
                    [ row
                        [ alignLeft
                        , spacing size4
                        , paddingEach { each0 | right = size4 }
                        , Border.solid
                        , Border.widthEach { each0 | right = 1 }
                        , Border.color textColourGrey
                        ]
                      <|
                        List.repeat (player.discCount - placedDiscCount) unknownDiscDisplay
                    , row
                        [ alignRight
                        , spacing size4
                        ]
                      <|
                        List.append
                            (List.map discDisplay revealedDiscs)
                            (List.repeat unrevealedDiscCount unknownDiscDisplay)
                    ]
                ]


statusMessage : String -> Element Msg
statusMessage message =
    row
        [ width fill
        , padding size4
        , spacing size4
        , Background.color colourWhite
        , Border.widthEach { each0 | left = size4 }
        , Border.color colourBlack
        ]
        [ el [] <|
            Element.html
                (Icon.infoCircle
                    |> Icon.present
                    |> Icon.view
                )
        , paragraph
            [ Font.alignLeft ]
            [ text message ]
        ]


discDisplay : Disc -> Element Msg
discDisplay disc =
    let
        discTypeEl =
            case disc of
                Thorn ->
                    Element.html
                        (Icon.biohazard
                            |> Icon.present
                            |> Icon.styled [ Html.Attributes.style "color" <| formatColor colourBlack2 ]
                            |> Icon.view
                        )

                Rose ->
                    Element.html
                        (Icon.spa
                            |> Icon.present
                            |> Icon.styled [ Html.Attributes.style "color" <| formatColor colourHighlight ]
                            |> Icon.view
                        )
    in
    el
        [ width <| px 40
        , height <| px 40
        , Border.rounded 20
        , Background.color colourAlt
        , Border.solid
        , Border.color textColourFeature
        , Font.color textColourDark
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
        , Font.color textColourDark
        , centerY
        ]
    <|
        el [ centerY, centerX ] <|
            Element.html
                (Icon.question
                    |> Icon.present
                    |> Icon.styled [ Html.Attributes.style "color" <| formatColor textColourFeature ]
                    |> Icon.view
                )


uiHook : String -> Attribute Msg
uiHook name =
    -- Allows automated testing to target the element
    Element.htmlAttribute <| Html.Attributes.class <| "ui-hook--" ++ name
