module Common exposing (mapSelected)

import SelectList exposing (SelectList, Position(..), mapBy)


mapSelected : (a -> a) -> SelectList a -> SelectList a
mapSelected f s =
    s
        |> mapBy
            (\position elem ->
                case position of
                    Selected ->
                        f elem

                    _ ->
                        elem
            )
