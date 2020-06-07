module Views.GameLogic exposing (..)

import Model exposing (Game, Self)


isCreator : Game -> Self -> Bool
isCreator game self =
    self.playerId == game.creatorId
