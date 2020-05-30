module TestUtils exposing (..)

import Expect exposing (Expectation, fail, pass)


okResult : (b -> Expectation) -> Result a b -> Expectation
okResult fn result =
    case result of
        Ok b ->
            fn b

        Err _ ->
            fail "expected `Ok _`, got Err"
