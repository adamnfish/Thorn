module Views.GameLogic exposing (..)

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
