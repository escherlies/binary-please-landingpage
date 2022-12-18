module Utils exposing (..)

{-| Extra stuff like helpers and utility functions
-}

-- Combinators


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
fgxy__fx_gy : (a -> c -> d) -> (b -> c) -> a -> b -> d
fgxy__fx_gy f g x y =
    f x (g y)
