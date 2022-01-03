module Game.Internal exposing (..)

import Grid exposing (Grid)
import Types exposing (..)


generateListOfPossibleIndizes : Grid InitGameCell -> Coordinates -> List Int
generateListOfPossibleIndizes initGrid clickedOn =
    let
        gridWidth =
            Grid.width initGrid

        gridHeight =
            Grid.height initGrid

        foldFn : Maybe Int -> List Int -> List Int
        foldFn x acc =
            case x of
                Nothing ->
                    acc

                Just idx ->
                    idx :: acc

        openingAreaCoordinates =
            List.range (max 0 clickedOn.x - 1) (min (gridWidth - 1) (clickedOn.x + 1))
                |> List.concatMap (\x -> List.map (\y -> { x = x, y = y }) <| List.range (max 0 clickedOn.y - 1) (min (gridHeight - 1) (clickedOn.y + 1)))
    in
    Grid.indexedMap
        (\x y _ ->
            if List.member { x = x, y = y } openingAreaCoordinates then
                Nothing

            else
                Just (x + gridWidth * y)
        )
        initGrid
        |> Grid.foldr foldFn []
