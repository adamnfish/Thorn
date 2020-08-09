module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Json.Decode exposing (errorToString)
import Model as Msg exposing (..)
import Msg exposing (parsePersistedGames, sendWake, update)
import Ports exposing (receiveMessage, reportError, socketConnect, socketDisconnect)
import Task
import Time
import Views.Ui exposing (view)


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( initialLibrary, parseLibraryCmd ) =
            parsePersistedGames flags.savedGames

        initial : Model
        initial =
            { library = initialLibrary
            , connected = False
            , ui = HomeScreen
            , errors = []
            , now = Time.millisToPosix 0
            , viewport =
                { scene =
                    { width = 0
                    , height = 0
                    }
                , viewport =
                    { x = 0
                    , y = 0
                    , width = 360
                    , height = 640
                    }
                }
            , hideSecrets = False
            }
    in
    ( initial
    , Cmd.batch
        [ Task.perform Msg.Tick Time.now
        , sendWake ()
        , Task.perform Resized Browser.Dom.getViewport
        , parseLibraryCmd
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveMessage ServerMessage
        , socketConnect <| always SocketConnect
        , socketDisconnect <| always SocketDisconnect
        , Time.every 1000 Tick
        , Browser.Events.onResize (\_ _ -> OnResize)
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
