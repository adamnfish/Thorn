module Main exposing (..)

import Browser
import Dict
import Model exposing (..)
import Msg exposing (update)
import Ports exposing (receiveMessage)
import Views.Ui exposing (view)


init : ( Model, Cmd Msg )
init =
    ( { library = Dict.empty
      , current = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveMessage ServerMessage



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
