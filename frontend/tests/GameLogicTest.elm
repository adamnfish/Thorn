module GameLogicTest exposing (..)

import Dict
import Expect
import GameLogic exposing (allFlipped)
import Model exposing (BiddingData, Disc(..), FlippingData, Game, GameId(..), PlayerId(..), Round(..), Self)
import Test exposing (Test, describe, test)


all : Test
all =
    let
        roundData : FlippingData
        roundData =
            { activePlayer = Pid "creator-id"
            , target = 8
            , bids =
                Dict.empty
            , discs =
                Dict.fromList
                    [ ( "creator-id", 2 )
                    , ( "player-1-id", 2 )
                    , ( "player-2-id", 2 )
                    , ( "player-3-id", 2 )
                    ]
            , revealed =
                Dict.fromList
                    [ ( "creator-id", [ Rose, Rose ] )
                    , ( "player-1-id", [ Rose ] )
                    , ( "player-2-id", [] )
                    , ( "player-3-id", [ Rose, Rose ] )
                    ]
            }

        game : Game
        game =
            { gameId = Gid "game-id"
            , gameName = "game name"
            , creatorId = Pid "creator-id"
            , players =
                [ { screenName = "creator name"
                  , playerId = Pid "creator-id"
                  , score = 0
                  , discCount = 4
                  }
                , { screenName = "player 2 name"
                  , playerId = Pid "player-2-id"
                  , score = 0
                  , discCount = 4
                  }
                , { screenName = "player 3 name"
                  , playerId = Pid "player-3-id"
                  , score = 0
                  , discCount = 4
                  }
                , { screenName = "player 4 name"
                  , playerId = Pid "player-4-id"
                  , score = 0
                  , discCount = 4
                  }
                ]
            , round =
                Just <| Flipping roundData
            , started = False
            }

        self : Self
        self =
            { screenName = "creator name"
            , playerId = Pid "creator-id"
            , score = 0
            , placedDiscs = Just []
            , roseCount = 3
            , hasThorn = True
            }

        gameStatus =
            { game = game
            , self = self
            }
    in
    describe "game logic"
        [ describe "allFlipped"
            [ test "returns True for self when all own discs flipped" <|
                \_ ->
                    allFlipped gameStatus (Pid "creator-id") |> Expect.equal True
            , test "returns True for another player who has had all discs flipped" <|
                \_ ->
                    allFlipped gameStatus (Pid "player-3-id") |> Expect.equal True
            , test "returns False for another player who has had no discs flipped" <|
                \_ ->
                    allFlipped gameStatus (Pid "player-2-id") |> Expect.equal False
            , test "returns False for another player who has had some (not all) discs flipped" <|
                \_ ->
                    allFlipped gameStatus (Pid "player-1-id") |> Expect.equal False
            ]
        ]
