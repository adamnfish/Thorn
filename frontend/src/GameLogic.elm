module GameLogic exposing (..)

import Dict
import Model exposing (Game, GameStatusMessage, Round(..), Self)


isCreator : Game -> Self -> Bool
isCreator game self =
    self.playerId == game.creatorId


isActive : GameStatusMessage -> Bool
isActive gameStatus =
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
