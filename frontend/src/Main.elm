module Main exposing (..)

import Browser
import Model exposing (..)
import Msg exposing (update)
import Views.Ui exposing (view)


init : ( Model, Cmd Msg )
init =
    ( Home, Cmd.none )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
