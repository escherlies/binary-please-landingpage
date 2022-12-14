module Utils exposing (..)

{-| Extra stuff like helpers and utility functions
-}


dropRight : Int -> List a -> List a
dropRight n ls =
    if n < 1 then
        ls

    else
        List.take (List.length ls - n) ls


liftResult : (error -> a) -> (value -> a) -> Result error value -> a
liftResult mapError mapValue result =
    case result of
        Ok value ->
            mapValue value

        Err error ->
            mapError error



-- Combinators


{-| Just an idea...

See <https://gist.github.com/escherlies/de92514ecdc295f0a098d41609d8677a>

-}
i_ : (c -> b -> a) -> b -> c -> a
i_ =
    flip


flip : (c -> b -> a) -> b -> c -> a
flip fn a b =
    fn b a


{-| Blackbird

    \a b c d -> a (b c d)

1˚ <- 2˚ composition

-}
b1 : (d -> c) -> (a -> b -> d) -> a -> b -> c
b1 =
    (<<) << (<<)


{-| λabcd.ac(bd)

Point-free or die!

-}
verschränkt : (a -> c -> d) -> (b -> c) -> a -> b -> d
verschränkt f g x y =
    f x (g y)
