module Pages.NotFound exposing (view)

import Element exposing (..)
import Element.Font as Font
import Session exposing (Session)
import Views exposing (headerView)


view : Maybe Session -> Element msg
view session =
    column
        [ width fill
        , height fill
        ]
        [ headerView session
        , el [ centerX, centerY, Font.size 40 ] <| text "Page Not Found"
        ]
