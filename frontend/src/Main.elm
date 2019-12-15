module Main exposing (main)

import Bootstrap.Grid as Grid
import Html exposing (..)


main : Html msg
main =
    Grid.container []
        [ Grid.row []
            [ Grid.col
                []
                [ text "Hello world" ]
            ]
        ]
