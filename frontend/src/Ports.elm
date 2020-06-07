port module Ports exposing (..)

import Json.Encode


port receiveMessage : (Json.Encode.Value -> msg) -> Sub msg


port socketConnect : (Maybe Int -> msg) -> Sub msg


port socketDisconnect : (Maybe Int -> msg) -> Sub msg


port sendMessage : Json.Encode.Value -> Cmd msg


port reportError : String -> Cmd msg
