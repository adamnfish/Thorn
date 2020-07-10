module GameLogic exposing (..)

import Dict
import List.Extra
import Model exposing (Disc(..), Game, GameStatusMessage, Player, PlayerId, Round(..), Self, getPid)


isCreator : Game -> Self -> Bool
isCreator game self =
    self.playerId == game.creatorId


selfIsActive : GameStatusMessage -> Bool
selfIsActive gameStatus =
    case gameStatus.game.round of
        Just (InitialDiscs _) ->
            let
                placedCount =
                    Maybe.map
                        (\placed -> List.length placed)
                        gameStatus.self.placedDiscs
                        |> Maybe.withDefault 0
            in
            placedCount == 0

        Just (Placing placingData) ->
            placingData.activePlayer == gameStatus.self.playerId

        Just (Bidding biddingData) ->
            biddingData.activePlayer == gameStatus.self.playerId

        Just (Flipping flippingData) ->
            flippingData.activePlayer == gameStatus.self.playerId

        Just (Finished _) ->
            False

        Nothing ->
            False


playerIsActive : GameStatusMessage -> Player -> Bool
playerIsActive gameStatus player =
    case gameStatus.game.round of
        Just (InitialDiscs initialDiscsData) ->
            let
                pid =
                    getPid player.playerId

                placedCount =
                    Maybe.withDefault 0 <|
                        Dict.get pid initialDiscsData.initialDiscs
            in
            placedCount == 0

        Just (Placing placingData) ->
            placingData.activePlayer == player.playerId

        Just (Bidding biddingData) ->
            biddingData.activePlayer == player.playerId

        Just (Flipping flippingData) ->
            flippingData.activePlayer == player.playerId

        Just (Finished _) ->
            False

        Nothing ->
            False


numberOfPlacedDiscs : GameStatusMessage -> Int
numberOfPlacedDiscs gameStatus =
    case gameStatus.game.round of
        Just (InitialDiscs initialDiscsData) ->
            List.sum <| Dict.values initialDiscsData.initialDiscs

        Just (Placing placingData) ->
            List.sum <| Dict.values placingData.discs

        Just (Bidding biddingData) ->
            List.sum <| Dict.values biddingData.discs

        Just (Flipping flippingData) ->
            List.sum <| Dict.values flippingData.discs

        Just (Finished finishedData) ->
            List.sum <| Dict.values finishedData.discs

        Nothing ->
            0


hasPlacedThorn : GameStatusMessage -> Bool
hasPlacedThorn gameStatus =
    Maybe.withDefault False <|
        Maybe.map
            (List.member Thorn)
            gameStatus.self.placedDiscs


placedRoseCount : GameStatusMessage -> Int
placedRoseCount gameStatus =
    Maybe.withDefault 0 <|
        Maybe.map
            (List.Extra.count (\d -> d == Rose))
            gameStatus.self.placedDiscs


minBid : GameStatusMessage -> Int
minBid gameStatus =
    case gameStatus.game.round of
        Just (InitialDiscs _) ->
            1

        Just (Placing _) ->
            1

        Just (Bidding biddingData) ->
            Maybe.withDefault 1 <| List.maximum <| List.map (\b -> b + 1) <| Dict.values biddingData.bids

        Just (Flipping flippingData) ->
            Maybe.withDefault 1 <| List.maximum <| List.map (\b -> b + 1) <| Dict.values flippingData.bids

        Just (Finished _) ->
            1

        Nothing ->
            1


allFlipped : GameStatusMessage -> PlayerId -> Bool
allFlipped gameStatus playerId =
    case gameStatus.game.round of
        Just (InitialDiscs _) ->
            False

        Just (Placing _) ->
            False

        Just (Bidding _) ->
            False

        Just (Flipping flippingData) ->
            let
                maybePlayerDiscCount =
                    Dict.get (getPid playerId) flippingData.discs

                maybePlayerRevealedCount =
                    Maybe.map List.length <|
                        Dict.get (getPid playerId) flippingData.revealed
            in
            Maybe.withDefault False <| Maybe.map2 (>=) maybePlayerRevealedCount maybePlayerDiscCount

        Just (Finished _) ->
            False

        Nothing ->
            False


gameWinner : GameStatusMessage -> Maybe Player
gameWinner gameStatus =
    List.Extra.find
        (\player -> player.score == 2)
        gameStatus.game.players


selfAsPlayer : Self -> Player
selfAsPlayer self =
    let
        thornCount =
            if self.hasThorn then
                1

            else
                0

        discCount =
            thornCount + self.roseCount
    in
    { screenName = self.screenName ++ " (you)"
    , playerId = self.playerId
    , score = self.score
    , discCount = discCount
    }
