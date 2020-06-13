module Model exposing (..)

import Browser.Dom exposing (Viewport)
import Dict exposing (Dict)
import Json.Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode
import Time


type Msg
    = NoOp
    | Tick Time.Posix
    | OnResize
    | Resized Viewport
      -- Connection status and server messages
    | ServerMessage Json.Encode.Value
    | SocketConnect
    | SocketDisconnect
    | NavigateHome
    | NavigateGame GameStatusMessage WelcomeMessage
      -- create game
    | NavigateCreateGame
    | InputCreateGame String String LoadingStatus
    | SubmitCreateGame String String
      -- join game
    | NavigateJoinGame
    | InputJoinGame String String LoadingStatus
    | SubmitJoinGame String String
      -- lobby
    | InputReorderPlayers (List Player)
    | SubmitStartGame
      -- in-game messages
    | InputPlaceDisc Disc
    | InputRemovePlaceDisc
    | SubmitPlaceDisc Disc
    | InputBid Int
    | InputRemoveBid
    | SubmitBid Int
    | InputPass
    | InputRemovePass
    | SubmitPass
    | InputFlip PlayerId
    | InputRemoveFlip
    | SubmitFlip PlayerId
    | SubmitNewRound


type alias Model =
    { library : Dict String GameInProgress
    , connected : Bool
    , ui : UI
    , errors : List UIError
    , now : Time.Posix
    , viewport : Maybe Viewport
    }


type UI
    = HomeScreen
    | CreateGameScreen String String LoadingStatus
    | JoinGameScreen String String LoadingStatus
    | LobbyScreen (List Player) WelcomeMessage LoadingStatus
    | PlaceDiscScreen (Maybe Disc) GameStatusMessage WelcomeMessage LoadingStatus
    | DiscOrBidScreen DiscOrBid GameStatusMessage WelcomeMessage LoadingStatus
    | BidOrPassScreen BidOrPass GameStatusMessage WelcomeMessage LoadingStatus
    | FlipScreen (Maybe PlayerId) GameStatusMessage WelcomeMessage LoadingStatus
    | DisplayGameScreen GameStatusMessage WelcomeMessage


type LoadingStatus
    = NotLoading
    | AwaitingMessage


type alias UIError =
    { message : Failure
    , time : Time.Posix
    }


type GameInProgress
    = Waiting WelcomeMessage (List Player)
    | Playing GameStatusMessage WelcomeMessage
    | NotPlaying GameStatusMessage


type DiscOrBid
    = DiscOrBidDisc Disc
    | DiscOrBidBid Int
    | DiscOrBidNoSelection


type BidOrPass
    = BidOrPassBid Int
    | BidOrPassPass
    | BidOrPassNoSelection


type GameId
    = Gid String


type PlayerKey
    = Pkey String


type PlayerId
    = Pid String


type Disc
    = Thorn
    | Rose


type Round
    = InitialDiscs InitialDiscsData
    | Placing PlacingData
    | Bidding BiddingData
    | Flipping FlippingData
    | Finished FinishedData


type alias InitialDiscsData =
    { activePlayer : PlayerId
    , initialDiscs : Dict String Int
    }


type alias PlacingData =
    { activePlayer : PlayerId
    , discs : Dict String Int
    }


type alias BiddingData =
    { activePlayer : PlayerId
    , discs : Dict String Int
    , bids : Dict String Int
    , passed : List PlayerId
    }


type alias FlippingData =
    { activePlayer : PlayerId
    , target : Int
    , bids : Dict String Int
    , discs : Dict String Int
    , revealed : Dict String (List Disc)
    }


type alias FinishedData =
    { activePlayer : PlayerId
    , discs : Dict String Int
    , revealed : Dict String (List Disc)
    , successful : Bool
    }


type alias Self =
    { screenName : String
    , playerId : PlayerId
    , score : Int
    , placedDiscs : Maybe (List Disc)
    , roseCount : Int
    , hasThorn : Bool
    }


type alias Player =
    { screenName : String
    , playerId : PlayerId
    , score : Int
    , discCount : Int
    }


type alias Game =
    { gameId : GameId
    , gameName : String
    , creatorId : PlayerId
    , players : List Player
    , round : Maybe Round
    , started : Bool
    }



-- requests


type alias CreateGame =
    { gameName : String
    , screenName : String
    }


type alias JoinGame =
    { gameCode : String
    , screenName : String
    }


type alias StartGame =
    { gameId : GameId
    , playerId : PlayerId
    , playerKey : PlayerKey
    , playerOrder : List PlayerId
    }


type alias PlaceDisc =
    { gameId : GameId
    , playerId : PlayerId
    , playerKey : PlayerKey
    , disc : Disc
    }


type alias Bid =
    { gameId : GameId
    , playerId : PlayerId
    , playerKey : PlayerKey
    , count : Int
    }


type alias Pass =
    { gameId : GameId
    , playerId : PlayerId
    , playerKey : PlayerKey
    }


type alias Flip =
    { gameId : GameId
    , playerId : PlayerId
    , playerKey : PlayerKey
    , stack : PlayerId
    }


type alias NewRound =
    { gameId : GameId
    , playerId : PlayerId
    , playerKey : PlayerKey
    }


type alias Reconnect =
    { gameId : GameId
    , playerId : PlayerId
    , playerKey : PlayerKey
    }


type alias Ping =
    { gameId : GameId
    , playerId : PlayerId
    , playerKey : PlayerKey
    }



-- messages


type Message
    = Status StatusMessage
    | Welcome WelcomeMessage
    | GameStatus GameStatusMessage
    | FailedAttempt (List Failure)


type alias StatusMessage =
    { message : String
    }


type alias WelcomeMessage =
    { playerKey : PlayerKey
    , playerId : PlayerId
    , gameId : GameId
    }


type alias GameStatusMessage =
    { self : Self
    , game : Game
    }


type alias Failure =
    { friendlyMessage : String
    , statusCode : Int
    , context : Maybe String
    }



-- Extractors


getGid : GameId -> String
getGid (Gid gameId) =
    gameId


getPid : PlayerId -> String
getPid (Pid playerId) =
    playerId


gameCode : GameId -> String
gameCode (Gid gameId) =
    String.left 4 gameId



-- JSON Decoders


playerKeyDecoder : Json.Decode.Decoder PlayerKey
playerKeyDecoder =
    Json.Decode.map Pkey Json.Decode.string


playerIdDecoder : Json.Decode.Decoder PlayerId
playerIdDecoder =
    Json.Decode.map Pid Json.Decode.string


gameIdDecoder : Json.Decode.Decoder GameId
gameIdDecoder =
    Json.Decode.map Gid Json.Decode.string


failureDecoder : Json.Decode.Decoder Failure
failureDecoder =
    Json.Decode.succeed Failure
        |> required "friendlyMessage" Json.Decode.string
        |> required "statusCode" Json.Decode.int
        |> optional "context" (Json.Decode.nullable Json.Decode.string) Nothing


messageDecoder : Json.Decode.Decoder Message
messageDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map Welcome welcomeMessageDecoder
        , Json.Decode.map GameStatus gameStatusMessageDecoder
        , Json.Decode.map Status statusMessageDecoder
        , Json.Decode.map FailedAttempt failedAttemptDecoder
        ]


statusMessageDecoder : Json.Decode.Decoder StatusMessage
statusMessageDecoder =
    Json.Decode.succeed StatusMessage
        |> required "message" Json.Decode.string


welcomeMessageDecoder : Json.Decode.Decoder WelcomeMessage
welcomeMessageDecoder =
    Json.Decode.succeed WelcomeMessage
        |> required "playerKey" playerKeyDecoder
        |> required "playerId" playerIdDecoder
        |> required "gameId" gameIdDecoder


gameStatusMessageDecoder : Json.Decode.Decoder GameStatusMessage
gameStatusMessageDecoder =
    Json.Decode.succeed GameStatusMessage
        |> required "self" selfDecoder
        |> required "game" gameDecoder


failedAttemptDecoder : Json.Decode.Decoder (List Failure)
failedAttemptDecoder =
    Json.Decode.field "failures" <|
        Json.Decode.list failureDecoder


discDecoder : Json.Decode.Decoder Disc
discDecoder =
    let
        get id =
            case id of
                "thorn" ->
                    Json.Decode.succeed Thorn

                "rose" ->
                    Json.Decode.succeed Rose

                _ ->
                    Json.Decode.fail ("unknown value for Disc: " ++ id)
    in
    Json.Decode.string |> Json.Decode.andThen get


selfDecoder : Json.Decode.Decoder Self
selfDecoder =
    Json.Decode.succeed Self
        |> required "screenName" Json.Decode.string
        |> required "playerId" playerIdDecoder
        |> required "score" Json.Decode.int
        |> required "placedDiscs" (Json.Decode.nullable (Json.Decode.list discDecoder))
        |> required "roseCount" Json.Decode.int
        |> required "hasThorn" Json.Decode.bool


playerDecoder : Json.Decode.Decoder Player
playerDecoder =
    Json.Decode.succeed Player
        |> required "screenName" Json.Decode.string
        |> required "playerId" playerIdDecoder
        |> required "score" Json.Decode.int
        |> required "discCount" Json.Decode.int


initialDiscsDecoder : Json.Decode.Decoder InitialDiscsData
initialDiscsDecoder =
    Json.Decode.succeed InitialDiscsData
        |> required "activePlayer" playerIdDecoder
        |> required "initialDiscs" (Json.Decode.dict Json.Decode.int)


placingDecoder : Json.Decode.Decoder PlacingData
placingDecoder =
    Json.Decode.succeed PlacingData
        |> required "activePlayer" playerIdDecoder
        |> required "discs" (Json.Decode.dict Json.Decode.int)


biddingDecoder : Json.Decode.Decoder BiddingData
biddingDecoder =
    Json.Decode.succeed BiddingData
        |> required "activePlayer" playerIdDecoder
        |> required "discs" (Json.Decode.dict Json.Decode.int)
        |> required "bids" (Json.Decode.dict Json.Decode.int)
        |> required "passed" (Json.Decode.list playerIdDecoder)


flippingDecoder : Json.Decode.Decoder FlippingData
flippingDecoder =
    Json.Decode.succeed FlippingData
        |> required "activePlayer" playerIdDecoder
        |> required "target" Json.Decode.int
        |> required "bids" (Json.Decode.dict Json.Decode.int)
        |> required "discs" (Json.Decode.dict Json.Decode.int)
        |> required "revealed" (Json.Decode.dict (Json.Decode.list discDecoder))


finishedDecoder : Json.Decode.Decoder FinishedData
finishedDecoder =
    Json.Decode.succeed FinishedData
        |> required "activePlayer" playerIdDecoder
        |> required "discs" (Json.Decode.dict Json.Decode.int)
        |> required "revealed" (Json.Decode.dict (Json.Decode.list discDecoder))
        |> required "successful" Json.Decode.bool


roundDecoder : Json.Decode.Decoder Round
roundDecoder =
    Json.Decode.field "round" Json.Decode.string
        |> Json.Decode.andThen roundDataDecoder


roundDataDecoder : String -> Json.Decode.Decoder Round
roundDataDecoder roundMarker =
    case roundMarker of
        "initialdiscs" ->
            Json.Decode.map InitialDiscs initialDiscsDecoder

        "placing" ->
            Json.Decode.map Placing placingDecoder

        "bidding" ->
            Json.Decode.map Bidding biddingDecoder

        "flipping" ->
            Json.Decode.map Flipping flippingDecoder

        "finished" ->
            Json.Decode.map Finished finishedDecoder

        _ ->
            Json.Decode.fail ("unrecognised round type: " ++ roundMarker)


gameDecoder : Json.Decode.Decoder Game
gameDecoder =
    Json.Decode.succeed Game
        |> required "gameId" gameIdDecoder
        |> required "gameName" Json.Decode.string
        |> required "creatorId" playerIdDecoder
        |> required "players" (Json.Decode.list playerDecoder)
        |> required "round" (Json.Decode.nullable roundDecoder)
        |> required "started" Json.Decode.bool



-- JSON Encoders


encodeGameId : GameId -> Json.Encode.Value
encodeGameId (Gid string) =
    Json.Encode.string string


encodePlayerId : PlayerId -> Json.Encode.Value
encodePlayerId (Pid string) =
    Json.Encode.string string


encodePlayerKey : PlayerKey -> Json.Encode.Value
encodePlayerKey (Pkey string) =
    Json.Encode.string string


encodeDisc : Disc -> Json.Encode.Value
encodeDisc disc =
    case disc of
        Thorn ->
            Json.Encode.string "thorn"

        Rose ->
            Json.Encode.string "rose"


createGameEncoder : CreateGame -> Json.Encode.Value
createGameEncoder createGame =
    Json.Encode.object <|
        [ ( "gameName", Json.Encode.string createGame.gameName )
        , ( "screenName", Json.Encode.string createGame.screenName )
        , ( "operation", Json.Encode.string "create-game" )
        ]


joinGameEncoder : JoinGame -> Json.Encode.Value
joinGameEncoder joinGame =
    Json.Encode.object <|
        [ ( "gameCode", Json.Encode.string joinGame.gameCode )
        , ( "screenName", Json.Encode.string joinGame.screenName )
        , ( "operation", Json.Encode.string "join-game" )
        ]


startGameEncoder : StartGame -> Json.Encode.Value
startGameEncoder startGame =
    Json.Encode.object <|
        [ ( "gameId", encodeGameId startGame.gameId )
        , ( "playerId", encodePlayerId startGame.playerId )
        , ( "playerKey", encodePlayerKey startGame.playerKey )
        , ( "playerOrder", Json.Encode.list encodePlayerId startGame.playerOrder )
        , ( "operation", Json.Encode.string "start-game" )
        ]


placeDiscEncoder : PlaceDisc -> Json.Encode.Value
placeDiscEncoder placeDisc =
    Json.Encode.object <|
        [ ( "gameId", encodeGameId placeDisc.gameId )
        , ( "playerId", encodePlayerId placeDisc.playerId )
        , ( "playerKey", encodePlayerKey placeDisc.playerKey )
        , ( "disc", encodeDisc placeDisc.disc )
        , ( "operation", Json.Encode.string "place-disc" )
        ]


bidEncoder : Bid -> Json.Encode.Value
bidEncoder bid =
    Json.Encode.object <|
        [ ( "gameId", encodeGameId bid.gameId )
        , ( "playerId", encodePlayerId bid.playerId )
        , ( "playerKey", encodePlayerKey bid.playerKey )
        , ( "count", Json.Encode.int bid.count )
        , ( "operation", Json.Encode.string "bid" )
        ]


passEncoder : Pass -> Json.Encode.Value
passEncoder pass =
    Json.Encode.object <|
        [ ( "gameId", encodeGameId pass.gameId )
        , ( "playerId", encodePlayerId pass.playerId )
        , ( "playerKey", encodePlayerKey pass.playerKey )
        , ( "operation", Json.Encode.string "pass" )
        ]


flipEncoder : Flip -> Json.Encode.Value
flipEncoder flip =
    Json.Encode.object <|
        [ ( "gameId", encodeGameId flip.gameId )
        , ( "playerId", encodePlayerId flip.playerId )
        , ( "playerKey", encodePlayerKey flip.playerKey )
        , ( "stack", encodePlayerId flip.stack )
        , ( "operation", Json.Encode.string "flip" )
        ]


newRoundEncoder : NewRound -> Json.Encode.Value
newRoundEncoder newRound =
    Json.Encode.object <|
        [ ( "gameId", encodeGameId newRound.gameId )
        , ( "playerId", encodePlayerId newRound.playerId )
        , ( "playerKey", encodePlayerKey newRound.playerKey )
        , ( "operation", Json.Encode.string "new-round" )
        ]


reconnectEncoder : Reconnect -> Json.Encode.Value
reconnectEncoder reconnect =
    Json.Encode.object <|
        [ ( "gameId", encodeGameId reconnect.gameId )
        , ( "playerId", encodePlayerId reconnect.playerId )
        , ( "playerKey", encodePlayerKey reconnect.playerKey )
        , ( "operation", Json.Encode.string "reconnect" )
        ]


pingEncoder : Ping -> Json.Encode.Value
pingEncoder ping =
    Json.Encode.object <|
        [ ( "gameId", encodeGameId ping.gameId )
        , ( "playerId", encodePlayerId ping.playerId )
        , ( "playerKey", encodePlayerKey ping.playerKey )
        , ( "operation", Json.Encode.string "ping" )
        ]


wakeEncoder : () -> Json.Encode.Value
wakeEncoder _ =
    Json.Encode.object <|
        [ ( "operation", Json.Encode.string "wake" )
        ]
