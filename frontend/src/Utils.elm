module Utils exposing (..)


flip : (a -> b -> c) -> (b -> a -> c)
flip fn b a =
    fn a b


reorderToBy : (x -> y) -> y -> List x -> List x
reorderToBy f y xs =
    let
        loop remainder acc =
            case remainder of
                [] ->
                    List.reverse acc

                head :: tail ->
                    if f head == y then
                        List.append remainder <| List.reverse acc

                    else
                        loop tail (head :: acc)
    in
    loop xs []


reorderTo : x -> List x -> List x
reorderTo =
    reorderToBy identity
