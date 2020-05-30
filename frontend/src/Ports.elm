port module Ports exposing (..)

import Json.Encode


port receiveMessage : (Json.Encode.Value -> msg) -> Sub msg


port socketConnect : (() -> msg) -> Sub msg


port sendMessage : Json.Encode.Value -> Cmd msg
