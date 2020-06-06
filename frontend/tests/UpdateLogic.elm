module UpdateLogic exposing (..)

import Expect exposing (Expectation)
import Model exposing (..)
import Msg exposing (includeAllPlayers)
import Test exposing (..)


all : Test
all =
    describe "update logic"
        [ describe "includeAllPlayers"
            [ test "preserves 1st arg's order if there are no new players" <|
                \_ ->
                    includeAllPlayers
                        [ player1, player2, player3 ]
                        [ player3, player1, player2 ]
                        |> Expect.equal [ player1, player2, player3 ]
            , test "adds new player to the bottom of the list" <|
                \_ ->
                    includeAllPlayers
                        [ player1, player2, player3 ]
                        [ player4, player3, player1, player2 ]
                        |> Expect.equal [ player1, player2, player3, player4 ]
            , test "adds multiple new players to the bottom of the list" <|
                \_ ->
                    includeAllPlayers
                        [ player1, player2, player3 ]
                        [ player4, player3, player5, player1, player2 ]
                        |> Expect.equal [ player1, player2, player3, player4, player5 ]
            ]
        ]


testPlayer : String -> Player
testPlayer identifier =
    { playerId = Pid identifier
    , screenName = identifier
    , score = 0
    , discCount = 4
    }


player1 : Player
player1 =
    testPlayer "player-1"


player2 : Player
player2 =
    testPlayer "player-2"


player3 : Player
player3 =
    testPlayer "player-3"


player4 : Player
player4 =
    testPlayer "player-4"


player5 : Player
player5 =
    testPlayer "player-5"
