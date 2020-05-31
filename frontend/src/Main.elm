module Main exposing (..)

import Browser
import Dict
import Model exposing (..)
import Msg exposing (update)
import Ports exposing (receiveMessage, socketConnect, socketDisconnect)
import Views.Ui exposing (view)


init : ( Model, Cmd Msg )
init =
    ( { library = Dict.empty
      , current = Nothing
      , connected = False
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveMessage ServerMessage
        , socketConnect <| always SocketConnect
        , socketDisconnect <| always SocketDisconnect
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
