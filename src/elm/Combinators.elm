module Combinators exposing (..)

{-| Combinators used in this project
-}


{-| Blackbird

    \a b c d -> a (b c d)

1˚ <- 2˚ composition

-}
b1 : (d -> c) -> (a -> b -> d) -> a -> b -> c
b1 =
    (<<) << (<<)


{-| Vireo Once Removed

    \a b c d -> a c (b d)

-}
vStar : (a -> d -> d) -> (b -> d) -> a -> b -> d
vStar a b c d =
    a c (b d)
