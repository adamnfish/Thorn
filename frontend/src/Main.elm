module Main exposing (..)

import Browser
import Dict
import Model as Msg exposing (..)
import Msg exposing (sendWake, update)
import Ports exposing (receiveMessage, socketConnect, socketDisconnect)
import Task
import Time
import Views.Ui exposing (view)


init : ( Model, Cmd Msg )
init =
    let
        initial : Model
        initial =
            { library = Dict.empty
            , connected = False
            , ui = HomeScreen
            , errors = []
            , now = Time.millisToPosix 0
            }
    in
    ( initial
    , Cmd.batch
        [ Task.perform Msg.Tick Time.now
        , sendWake ()
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveMessage ServerMessage
        , socketConnect <| always SocketConnect
        , socketDisconnect <| always SocketDisconnect
        , Time.every 1000 Tick
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
