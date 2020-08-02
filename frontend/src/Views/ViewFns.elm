module Views.ViewFns exposing (..)

import Browser.Dom exposing (Viewport)
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
import GameLogic exposing (allFlipped, playerIsActive, selfAsPlayer)
import Html.Attributes
import List.Extra
import Model exposing (Disc(..), GameStatusMessage, Model, Msg(..), Player, PlayerId, Round(..), getPid)
import Utils exposing (reorderToBy, swapDown, swapUp)
import Views.Model exposing (Controls, DiscDisplaySize(..), toPlayerDisplay)
import Views.Styles exposing (buttonStyles, colourBlack, colourCta, colourHighlight, colourHighlight2, colourSecondary, colourSecondary2, colourSecondaryHighlight, colourWhite, each0, featureButtonStyles, fontSizeSmall, formatColor, size1, size2, size3, size4, textColourDark, textColourGrey, textColourLight)


selfSecretInformation : Viewport -> Bool -> GameStatusMessage -> Element Msg
selfSecretInformation viewport hideSecrets gameStatus =
    let
        placed =
            Maybe.withDefault [] gameStatus.self.placedDiscs

        placedRoseCount =
            List.Extra.count (\d -> d == Rose) placed

        thornPoolEl =
            if gameStatus.self.hasThorn && not (List.member Thorn placed) then
                discDisplay viewport SmallDisc Thorn

            else
                Element.none

        rosePoolEls =
            List.map (discDisplay viewport SmallDisc) <|
                List.repeat (gameStatus.self.roseCount - placedRoseCount) Rose

        poolEls =
            List.reverse <| thornPoolEl :: rosePoolEls

        icon =
            if hideSecrets then
                Icon.eye

            else
                Icon.eyeSlash
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
            , Border.color colourBlack
            ]
        <|
            column
                [ width fill ]
                [ if not hideSecrets then
                    row
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
                            List.map (discDisplay viewport SmallDisc) placed
                        ]

                  else
                    Element.none
                , row
                    [ width fill
                    , padding size4
                    , fontSizeSmall
                    ]
                    [ text "Keep this secret"
                    , el
                        [ alignRight
                        ]
                      <|
                        Input.button
                            ((width <| px 40) :: buttonStyles)
                            { onPress = Just <| ToggleSecrets
                            , label =
                                Element.html
                                    (icon
                                        |> Icon.present
                                        |> Icon.view
                                    )
                            }
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


playersList : Viewport -> GameStatusMessage -> Bool -> Element Msg
playersList viewport gameStatus showStackSelector =
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
        List.map (playerPublicInformation viewport gameStatus showStackSelector) otherPlayers


selfPublicInformation : Viewport -> GameStatusMessage -> Element Msg
selfPublicInformation viewport gameStatus =
    let
        selfPlayer =
            selfAsPlayer gameStatus.self

        playerDisplay =
            toPlayerDisplay gameStatus selfPlayer

        icon =
            playerIcon textColourDark gameStatus.self.score

        unrevealedDiscEls =
            List.indexedMap
                (\i _ -> unknownDiscDisplay viewport <| LargeDisc i)
            <|
                List.repeat playerDisplay.unrevealedDiscCount Element.none

        revealedDiscEls =
            List.indexedMap
                (\i -> discDisplay viewport <| LargeDisc i)
                playerDisplay.revealedDiscs
    in
    el
        [ width fill
        , Background.color colourWhite
        , Border.widthEach
            { each0 | left = size4 }
        , Border.color colourHighlight
        , padding size4
        ]
    <|
        column
            [ spacing size4
            , width fill
            ]
            [ row
                [ spacing size4 ]
                [ icon
                , text ""
                , text playerDisplay.displayName
                ]
            , row
                []
              <|
                List.repeat playerDisplay.unplacedDiscCount <|
                    unknownDiscDisplay viewport SmallDisc
            , el
                [ inFront <|
                    row
                        [ width fill ]
                        [ row [] unrevealedDiscEls
                        , row
                            [ alignRight ]
                            revealedDiscEls
                        ]
                , height <| px 100
                , width fill
                ]
                Element.none
            ]


playerPublicInformation : Viewport -> GameStatusMessage -> Bool -> Player -> Element Msg
playerPublicInformation viewport gameStatus showStackSelector player =
    let
        playerDisplay =
            toPlayerDisplay gameStatus player

        icon =
            playerIcon colourWhite player.score

        info =
            row
                [ width fill
                , spacing size4
                , height <| px 35
                ]
                [ el
                    [ width <| px 25 ]
                    icon
                , el
                    [ Font.color textColourLight ]
                  <|
                    text player.screenName
                , if playerDisplay.hasDiscsUnflipped && showStackSelector then
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
                if playerDisplay.active then
                    colourCta

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
                        List.repeat playerDisplay.unplacedDiscCount <|
                            unknownDiscDisplay viewport SmallDisc
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
                            (List.map (discDisplay viewport SmallDisc) playerDisplay.revealedDiscs)
                            (List.repeat playerDisplay.unrevealedDiscCount <| unknownDiscDisplay viewport SmallDisc)
                    ]
                ]


playerIcon : Color -> number -> Element msg
playerIcon colour score =
    if score > 0 then
        Element.html
            (Icon.crown
                |> Icon.present
                |> Icon.styled [ Html.Attributes.style "color" <| formatColor colour ]
                |> Icon.view
            )

    else
        el
            [ centerX ]
        <|
            Element.html
                (Icon.userCircle
                    |> Icon.present
                    |> Icon.styled [ Html.Attributes.style "color" <| formatColor colour ]
                    |> Icon.view
                )


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


discDisplay : Viewport -> DiscDisplaySize -> Disc -> Element Msg
discDisplay viewport discDisplaySize disc =
    let
        size =
            case discDisplaySize of
                SmallDisc ->
                    40

                LargeDisc i ->
                    100

        adjustment =
            case discDisplaySize of
                SmallDisc ->
                    0

                LargeDisc i ->
                    max
                        (toFloat <| 30 * i)
                        (toFloat i * (70 * 240 / viewport.viewport.width))

        borderRadius =
            round <| size / 2

        borderAttrs =
            case discDisplaySize of
                SmallDisc ->
                    []

                LargeDisc _ ->
                    [ Border.width size2
                    , Border.color colourWhite
                    ]

        fontSize =
            round <| size / 2

        discTypeEl =
            case disc of
                Thorn ->
                    Element.html
                        (Icon.biohazard
                            |> Icon.present
                            |> Icon.styled [ Html.Attributes.style "color" <| formatColor colourWhite ]
                            |> Icon.view
                        )

                Rose ->
                    Element.html
                        (Icon.spa
                            |> Icon.present
                            |> Icon.styled [ Html.Attributes.style "color" <| formatColor colourWhite ]
                            |> Icon.view
                        )
    in
    el
        ([ width <| px size
         , height <| px size
         , Border.rounded borderRadius
         , Background.color colourHighlight
         , Font.size fontSize
         , moveLeft adjustment
         ]
            ++ borderAttrs
        )
    <|
        el [ centerY, centerX ] <|
            discTypeEl


unknownDiscDisplay : Viewport -> DiscDisplaySize -> Element Msg
unknownDiscDisplay viewport discDisplaySize =
    let
        size =
            case discDisplaySize of
                SmallDisc ->
                    40

                LargeDisc i ->
                    100

        adjustment =
            case discDisplaySize of
                SmallDisc ->
                    0

                LargeDisc i ->
                    max
                        (toFloat <| 30 * i)
                        (toFloat i * (70 * 240 / viewport.viewport.width))

        borderRadius =
            round <| size / 2

        borderAttrs =
            case discDisplaySize of
                SmallDisc ->
                    []

                LargeDisc _ ->
                    [ Border.width size2
                    , Border.color colourWhite
                    ]

        fontSize =
            round <| size / 2
    in
    el
        ([ width <| px size
         , height <| px size
         , Border.rounded borderRadius
         , Background.color colourHighlight
         , Font.size fontSize
         , moveLeft adjustment
         ]
            ++ borderAttrs
        )
    <|
        el [ centerY, centerX ] <|
            Element.html
                (Icon.question
                    |> Icon.present
                    |> Icon.styled [ Html.Attributes.style "color" <| formatColor colourHighlight2 ]
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
