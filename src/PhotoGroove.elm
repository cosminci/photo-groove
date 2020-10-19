module PhotoGroove exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)


urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail thumb =
    img [ src (urlPrefix ++ thumb.filename) ] []


view model =
    div [ class "context" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            (List.map viewThumbnail model.thumbnails)
        ]


initialModel =
    { thumbnails =
        [ { filename = "1.jpeg" }
        , { filename = "2.jpeg" }
        , { filename = "3.jpeg" }
        ]
    }


main =
    view initialModel
