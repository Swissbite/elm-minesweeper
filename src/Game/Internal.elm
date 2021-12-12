module Game.Internal exposing (..)
import Grid exposing (Grid)
import Types exposing (..)

generateListOfPossibleIndizes : Grid InitGameCell -> Coordinates -> List Int
generateListOfPossibleIndizes initGrid clickedOn =
    let
        gridWidth = Grid.width initGrid

        foldFn : Maybe Int -> List Int -> List Int
        foldFn x acc =
            case x of
                Nothing ->
                    acc

                Just idx ->
                    idx :: acc
    in
    Grid.indexedMap
        (\x y _ ->
            if x == clickedOn.x && y == clickedOn.y then
                Nothing

            else
                Just (x + gridWidth * y)
        )
        initGrid
        |> Grid.foldr foldFn []

