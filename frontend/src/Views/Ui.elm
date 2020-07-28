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
import Views.Styles exposing (buttonStyles, centerBlock, colourAlt, colourAltSecondary, colourBlack, colourBlack2, colourCta, colourError, colourHighlight, colourHighlight2, colourPrimary, colourSecondary, colourSecondary2, colourSecondaryHighlight, colourSecondaryLight, colourWhite, each0, featureButtonStyles, fontSizeSmall, formatColor, length4, size1, size2, size3, size4, size5, size6, size7, spacer, textColourDark, textColourFeature, textColourGrey, textColourLight, textInputStyles)


type alias Page =
    { title : String
    , status : String
    , body : Element Msg
    , loading : LoadingStatus
    , ui : String
    }


type alias ConfirmControl =
    { confirm : Msg
    , cancel : Msg
    , description : Element Msg
    }


type alias Controls =
    { message : Maybe String
    , features : List ( Msg, Element Msg )
    , bids : Maybe ( Int, Int )
    , confirm : Maybe ConfirmControl
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
                    , body = placeDisc model gameStatus loadingStatus maybeDisc
                    , loading = loadingStatus
                    , ui = "place-disc"
                    }

                DiscOrBidScreen maybeSelection gameStatus _ loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , status = "Place disc or bid"
                    , body = discOrBid model gameStatus loadingStatus maybeSelection
                    , loading = loadingStatus
                    , ui = "disc-or-bid"
                    }

                BidOrPassScreen maybeInt gameStatus _ loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , status = "Bid or pass"
                    , body = bidOrPass model gameStatus loadingStatus maybeInt
                    , loading = loadingStatus
                    , ui = "bid-or-pass"
                    }

                FlipScreen maybeStack gameStatus _ loadingStatus ->
                    { title = gameStatus.game.gameName ++ " | Thorn"
                    , status = "Choose disc to flip"
                    , body = flip model gameStatus loadingStatus maybeStack
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
            [ width fill ]
            [ spacer 2
            , ctaCard <|
                column
                    [ width fill
                    , padding size6
                    , spacing size7
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
            ]


createGame : Model -> String -> String -> LoadingStatus -> Element Msg
createGame model gameName screenName loadingStatus =
    let
        controls =
            if loadingStatus == NotLoading then
                { emptyControls
                    | features =
                        [ ( SubmitCreateGame gameName screenName, text "Create game" ) ]
                }

            else
                { emptyControls
                    | message = Just "Loading..."
                }
    in
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
            , controlsEl controls
            ]


joinGame : Model -> String -> String -> LoadingStatus -> Element Msg
joinGame model gameCode screenName loadingStatus =
    let
        controls =
            if loadingStatus == NotLoading then
                { emptyControls
                    | features =
                        [ ( SubmitJoinGame gameCode screenName, text "Join game" ) ]
                }

            else
                { emptyControls
                    | message = Just "Loading..."
                }
    in
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
            , controlsEl controls
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

        controls =
            case maybeGameStatus of
                Just gameStatus ->
                    if isCreator gameStatus.game gameStatus.self then
                        if List.length playerOrder >= 2 then
                            if loadingStatus == NotLoading then
                                { emptyControls
                                    | features = [ ( SubmitStartGame, text "Start game" ) ]
                                    , message =
                                        if List.length playerOrder == 2 then
                                            Just "Thorn works best with at least 3 players"

                                        else
                                            Nothing
                                }

                            else
                                { emptyControls
                                    | message = Just "Loading..."
                                }

                        else
                            { emptyControls
                                | message = Just "Thorn needs at least 2 players"
                            }

                    else
                        { emptyControls
                            | message = Just "Waiting for game to start"
                        }

                Nothing ->
                    { emptyControls
                        | message = Just "Waiting for game to start"
                    }
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
            , spacer 2
            , controlsEl controls
            ]


placeDisc : Model -> GameStatusMessage -> LoadingStatus -> Maybe Disc -> Element Msg
placeDisc model gameStatus loadingStatus maybeDisc =
    let
        roseButton =
            if gameStatus.self.roseCount > placedRoseCount gameStatus then
                [ ( InputPlaceDisc Rose, text "Rose" ) ]

            else
                []

        thornButton =
            if gameStatus.self.hasThorn && (not <| hasPlacedThorn gameStatus) then
                [ ( InputPlaceDisc Thorn, text "Thorn" ) ]

            else
                []

        controls =
            case maybeDisc of
                Just disc ->
                    if loadingStatus == NotLoading then
                        { emptyControls
                            | confirm =
                                Just
                                    { confirm = SubmitPlaceDisc disc
                                    , cancel = InputRemovePlaceDisc
                                    , description = discDisplay disc
                                    }
                        }

                    else
                        { emptyControls
                            | message = Just "Loading..."
                        }

                Nothing ->
                    { emptyControls
                        | features =
                            List.append roseButton thornButton
                    }
    in
    centerBlock <|
        column
            [ width fill ]
            [ selfSecretInformation gameStatus
            , spacer 3
            , playersList gameStatus False
            , spacer 2
            , controlsEl controls
            ]


discOrBid : Model -> GameStatusMessage -> LoadingStatus -> DiscOrBid -> Element Msg
discOrBid model gameStatus loadingStatus maybeSelection =
    let
        roseButton =
            if gameStatus.self.roseCount > placedRoseCount gameStatus then
                [ ( InputPlaceDisc Rose, text "Rose" ) ]

            else
                []

        thornButton =
            if gameStatus.self.hasThorn && (not <| hasPlacedThorn gameStatus) then
                [ ( InputPlaceDisc Thorn, text "Thorn" ) ]

            else
                []

        controls =
            case maybeSelection of
                DiscOrBidDisc disc ->
                    if loadingStatus == NotLoading then
                        { emptyControls
                            | confirm =
                                Just
                                    { confirm = SubmitPlaceDisc disc
                                    , cancel = InputRemovePlaceDisc
                                    , description = discDisplay disc
                                    }
                        }

                    else
                        { emptyControls
                            | message = Just "Loading..."
                        }

                DiscOrBidBid bid ->
                    if loadingStatus == NotLoading then
                        { emptyControls
                            | confirm =
                                Just
                                    { confirm = SubmitBid bid
                                    , cancel = InputRemoveBid
                                    , description = text <| "Bid " ++ String.fromInt bid
                                    }
                        }

                    else
                        { emptyControls
                            | message = Just "Loading..."
                        }

                DiscOrBidNoSelection ->
                    { emptyControls
                        | features =
                            List.append roseButton thornButton
                        , bids = Just ( minBid gameStatus, numberOfPlacedDiscs gameStatus )
                    }
    in
    centerBlock <|
        column
            [ width fill ]
            [ selfSecretInformation gameStatus
            , spacer 3
            , playersList gameStatus False
            , spacer 2
            , controlsEl controls
            ]


bidOrPass : Model -> GameStatusMessage -> LoadingStatus -> BidOrPass -> Element Msg
bidOrPass model gameStatus loadingStatus maybeSelection =
    let
        controls =
            case maybeSelection of
                BidOrPassBid bid ->
                    if loadingStatus == NotLoading then
                        { emptyControls
                            | confirm =
                                Just
                                    { confirm = SubmitBid bid
                                    , cancel = InputRemoveBid
                                    , description = text <| "bid " ++ String.fromInt bid
                                    }
                        }

                    else
                        { emptyControls
                            | message = Just "Loading..."
                        }

                BidOrPassPass ->
                    if loadingStatus == NotLoading then
                        { emptyControls
                            | confirm =
                                Just
                                    { confirm = SubmitPass
                                    , cancel = InputRemovePass
                                    , description = text "Pass"
                                    }
                        }

                    else
                        { emptyControls
                            | message = Just "Loading..."
                        }

                BidOrPassNoSelection ->
                    { emptyControls
                        | features = [ ( InputPass, text "Pass" ) ]
                        , bids = Just ( minBid gameStatus, numberOfPlacedDiscs gameStatus )
                    }
    in
    centerBlock <|
        column
            [ width fill ]
            [ selfSecretInformation gameStatus
            , spacer 3
            , playersList gameStatus False
            , spacer 2
            , controlsEl controls
            ]


flip : Model -> GameStatusMessage -> LoadingStatus -> Maybe PlayerId -> Element Msg
flip model gameStatus loadingStatus maybeStack =
    let
        allOwnFlipped =
            allFlipped gameStatus gameStatus.self.playerId

        controls =
            case maybeStack of
                Just stackId ->
                    let
                        targetName =
                            lookupPlayerName gameStatus stackId
                    in
                    if loadingStatus == NotLoading then
                        { emptyControls
                            | confirm =
                                Just
                                    { confirm = SubmitFlip stackId
                                    , cancel = InputRemoveFlip
                                    , description = text <| "Flip " ++ targetName
                                    }
                        }

                    else
                        { emptyControls
                            | message = Just "Loading..."
                        }

                Nothing ->
                    if allOwnFlipped then
                        { emptyControls
                            | message = Just "Choose disc to flip"
                        }

                    else
                        { emptyControls
                            | features =
                                [ ( SubmitFlip gameStatus.self.playerId
                                  , text "Flip own discs"
                                  )
                                ]
                        }
    in
    centerBlock <|
        column
            [ width fill ]
            [ selfSecretInformation gameStatus
            , spacer 3
            , playersList gameStatus allOwnFlipped
            , spacer 2
            , controlsEl controls
            ]


currentGame : Model -> GameStatusMessage -> Element Msg
currentGame model gameStatus =
    let
        controls =
            case gameStatus.game.round of
                Just (InitialDiscs initialDiscs) ->
                    { emptyControls
                        | message = Just "Waiting for other players"
                    }

                Just (Placing placing) ->
                    let
                        actorName =
                            lookupPlayerName gameStatus placing.activePlayer
                    in
                    { emptyControls
                        | message = Just <| "Waiting for " ++ actorName
                    }

                Just (Bidding bidding) ->
                    let
                        actorName =
                            lookupPlayerName gameStatus bidding.activePlayer
                    in
                    { emptyControls
                        | message = Just <| "Waiting for " ++ actorName
                    }

                Just (Flipping flipping) ->
                    let
                        actorName =
                            lookupPlayerName gameStatus flipping.activePlayer
                    in
                    { emptyControls
                        | message = Just <| actorName ++ " is trying to win the round"
                    }

                Just (Finished finished) ->
                    case gameWinner gameStatus of
                        Just winner ->
                            if winner.playerId == gameStatus.self.playerId then
                                { emptyControls
                                    | message = Just "You have won the game"
                                }

                            else
                                { emptyControls
                                    | message =
                                        Just <|
                                            winner.screenName
                                                ++ " has won the game"
                                }

                        Nothing ->
                            let
                                actorName =
                                    lookupPlayerName gameStatus finished.activePlayer

                                newRound =
                                    if isCreator gameStatus.game gameStatus.self then
                                        [ ( SubmitNewRound, text "Next round" ) ]

                                    else
                                        []
                            in
                            if finished.successful then
                                if finished.activePlayer == gameStatus.self.playerId then
                                    { emptyControls
                                        | message = Just "You have won the round"
                                        , features = newRound
                                    }

                                else
                                    { emptyControls
                                        | message = Just <| actorName ++ " has won the round"
                                        , features = newRound
                                    }

                            else if finished.activePlayer == gameStatus.self.playerId then
                                { emptyControls
                                    | message = Just "You have hit a skull and failed to win the round"
                                    , features = newRound
                                }

                            else
                                { emptyControls
                                    | message = Just <| actorName ++ " failed to win the round"
                                    , features = newRound
                                }

                Nothing ->
                    { emptyControls
                        | message = Just "Waiting for the round to start"
                    }
    in
    centerBlock <|
        column
            [ width fill ]
            [ selfSecretInformation gameStatus
            , spacer 3
            , playersList gameStatus False
            , spacer 3
            , controlsEl controls
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
                        ]
                        poolEls
                    , row
                        [ spacing size4
                        , paddingEach { each0 | left = size4 }
                        , Border.solid
                        , Border.color textColourGrey
                        , Border.widthEach { each0 | left = 1 }
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
                        ]
                      <|
                        List.repeat (player.discCount - placedDiscCount) unknownDiscDisplay
                    , row
                        [ alignRight
                        , spacing size4
                        , paddingEach { each0 | left = size4 }
                        , Border.solid
                        , Border.color textColourGrey
                        , Border.widthEach { each0 | left = 1 }
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


ctaCard : Element Msg -> Element Msg
ctaCard contents =
    el
        [ width fill
        , padding size4
        , Background.color colourWhite
        , Border.widthEach
            { each0 | left = size4 }
        , Border.color colourCta
        ]
        contents


emptyControls : Controls
emptyControls =
    { message = Nothing
    , features = []
    , bids = Nothing
    , confirm = Nothing
    }


controlsEl : Controls -> Element Msg
controlsEl controls =
    ctaCard <|
        column
            [ width fill
            , spacing size4
            ]
            [ case controls.message of
                Just message ->
                    paragraph
                        [ Font.alignLeft
                        , width fill
                        , paddingXY 0 size3
                        ]
                        [ text message ]

                Nothing ->
                    Element.none
            , if List.isEmpty controls.features then
                Element.none

              else
                column
                    [ width fill
                    , spacing size4
                    ]
                <|
                    List.map
                        (\( msg, label ) ->
                            Input.button featureButtonStyles
                                { onPress = Just msg
                                , label = label
                                }
                        )
                        controls.features
            , case controls.bids of
                Just ( min, max ) ->
                    row
                        [ width fill
                        , spacing size4
                        ]
                    <|
                        List.map
                            (\bid ->
                                Input.button
                                    (List.append
                                        buttonStyles
                                        [ width fill ]
                                    )
                                    { onPress = Just <| InputBid bid
                                    , label = text <| String.fromInt bid
                                    }
                            )
                        <|
                            List.range min max

                Nothing ->
                    Element.none
            , case controls.confirm of
                Just confirmControl ->
                    column
                        [ width fill
                        , spacing size3
                        ]
                        [ el
                            [ centerX
                            , paddingXY 0 size3
                            ]
                            confirmControl.description
                        , row
                            [ width fill
                            , spacing size3
                            ]
                            [ Input.button
                                (List.append
                                    buttonStyles
                                    [ width <| fillPortion 2 ]
                                )
                                { onPress = Just confirmControl.cancel
                                , label =
                                    row
                                        [ width fill
                                        , spacing size4
                                        , padding size4
                                        ]
                                        [ el
                                            [ centerX ]
                                          <|
                                            text "Clear"
                                        , el
                                            [ centerX ]
                                          <|
                                            Element.html
                                                (Icon.times
                                                    |> Icon.present
                                                    |> Icon.view
                                                )
                                        ]
                                }
                            , Input.button
                                (List.append
                                    buttonStyles
                                    [ width <| fillPortion 3 ]
                                )
                                { onPress = Just confirmControl.confirm
                                , label =
                                    row
                                        [ width fill
                                        , spacing size4
                                        , padding size4
                                        ]
                                        [ el
                                            [ centerX ]
                                          <|
                                            text "Confirm"
                                        , el
                                            [ centerX ]
                                          <|
                                            Element.html
                                                (Icon.check
                                                    |> Icon.present
                                                    |> Icon.view
                                                )
                                        ]
                                }
                            ]
                        ]

                Nothing ->
                    Element.none
            ]


lookupPlayerName : GameStatusMessage -> PlayerId -> String
lookupPlayerName gameStatus playerId =
    Maybe.withDefault "another player" <|
        Maybe.map
            (\player -> player.screenName)
        <|
            List.Extra.find
                (\player -> player.playerId == playerId)
                gameStatus.game.players


uiHook : String -> Attribute Msg
uiHook name =
    -- Allows automated testing to target the element
    Element.htmlAttribute <| Html.Attributes.class <| "ui-hook--" ++ name
