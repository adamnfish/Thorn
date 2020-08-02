module Views.Model exposing (..)

import Dict
import Element exposing (Element)
import GameLogic exposing (allFlipped, playerIsActive)
import Model exposing (Disc, GameStatusMessage, LoadingStatus, Msg, Player, Round(..), getPid)


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


emptyControls : Controls
emptyControls =
    { message = Nothing
    , features = []
    , bids = Nothing
    , confirm = Nothing
    }


type alias PlayerDisplay =
    { active : Bool
    , revealedDiscs : List Disc
    , placedDiscCount : Int
    , unplacedDiscCount : Int
    , unrevealedDiscCount : Int
    , hasDiscsUnflipped : Bool
    , displayName : String
    , score : Int
    , isSelf : Bool
    }


toPlayerDisplay : GameStatusMessage -> Player -> PlayerDisplay
toPlayerDisplay gameStatus player =
    let
        pid =
            getPid player.playerId

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
    in
    { active = playerIsActive gameStatus player
    , revealedDiscs = revealedDiscs
    , placedDiscCount = placedDiscCount
    , unplacedDiscCount = player.discCount - placedDiscCount
    , unrevealedDiscCount = placedDiscCount - List.length revealedDiscs
    , hasDiscsUnflipped = not <| allFlipped gameStatus player.playerId
    , displayName = player.screenName
    , score = player.score
    , isSelf = player.playerId == gameStatus.self.playerId
    }


type DiscDisplaySize
    = SmallDisc
    | LargeDisc Int
