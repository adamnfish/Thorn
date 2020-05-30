module Views.Ui exposing (view)

import Browser
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Model exposing (..)


view : Model -> Browser.Document Msg
view model =
    { title = "Thorn"
    , body =
        [ layout [] <|
            el
                []
                (text "Hello, world!")
        ]
    }
