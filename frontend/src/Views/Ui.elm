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


type alias Page =
    { title : String
    , body : Element Msg
    , nav : Element Msg
    }


view : Model -> Browser.Document Msg
view model =
    let
        page : Page
        page =
            case model.ui of
                HomeScreen ->
                    { title = "Thorn"
                    , body = home model
                    , nav = Element.none
                    }

                CreateGameScreen gameName screenName loadingStatus ->
                    { title = "Create game | Thorn"
                    , body = createGame model gameName screenName loadingStatus
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    }

                JoinGameScreen gameCode screenName loadingStatus ->
                    { title = "Join game | Thorn"
                    , body = joinGame model gameCode screenName loadingStatus
                    , nav = nav [ ( NavigateHome, "Home" ) ]
                    }

                LobbyScreen playerOrder loadingStatus welcomeMessage ->
                    { title = "Waiting | Thorn"
                    , body = lobby model playerOrder loadingStatus welcomeMessage
                    , nav = Element.none
                    }

                CurrentGameScreen game self welcomeMessage ->
                    { title = game.gameName ++ " | Thorn"
                    , body = currentGame model game self welcomeMessage
                    , nav = Element.none
                    }
    in
    { title = page.title
    , body =
        [ layout [] <|
            Element.column []
                [ page.nav
                , page.body
                ]
        ]
    }


nav : List ( Msg, String ) -> Element Msg
nav navEntries =
    Element.row [] <|
        List.map
            (\( msg, label ) ->
                Input.button []
                    { onPress = Just msg
                    , label = text label
                    }
            )
            navEntries


home : Model -> Element Msg
home model =
    el [] <|
        column
            []
            [ Input.button []
                { onPress = Just NavigateCreateGame
                , label = text "Create game"
                }
            , Input.button []
                { onPress = Just NavigateJoinGame
                , label = text "Join game"
                }
            ]


createGame : Model -> String -> String -> LoadingStatus -> Element Msg
createGame model gameName screenName loadingStatus =
    el
        []
        (text "Create game")


joinGame : Model -> String -> String -> LoadingStatus -> Element Msg
joinGame model gameCode screenName loadingStatus =
    el
        []
        (text "Join game")


lobby : Model -> List Player -> LoadingStatus -> WelcomeMessage -> Element Msg
lobby model playerOrder loadingStatus welcomeMessage =
    el
        []
        (text "Lobby")


currentGame : Model -> Game -> Self -> WelcomeMessage -> Element Msg
currentGame model game self welcomeMessage =
    el
        []
        (text "showing game")
