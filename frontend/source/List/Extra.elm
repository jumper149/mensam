module List.Extra exposing (..)


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        head :: tail ->
            if predicate head then
                Just head

            else
                find predicate tail
