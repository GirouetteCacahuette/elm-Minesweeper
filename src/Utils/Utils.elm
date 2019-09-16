module Utils.Utils exposing (styles)

import Html exposing (Html, div, node)
import Html.Attributes exposing (href, rel)


styles : Html a
styles =
    div []
        [ node "link"
            [ rel "stylesheet"
            , href "/src/Utils/styles.css"
            ]
            []
        ]
