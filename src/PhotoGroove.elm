module PhotoGroove exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail selectedThumb thumb =
    img
        [ src (urlPrefix ++ thumb.fileName)
        , classList [ ( "selected", selectedThumb == thumb.fileName ) ]
        , onClick { action = constants.clickedPhoto, target = thumb.fileName }
        ]
        []


view model =
    div [ class "context" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            (List.map (viewThumbnail model.selectedThumbnail)
                model.thumbnails
            )
        , img [ class "large", src (urlPrefix ++ "large/" ++ model.selectedThumbnail) ] []
        ]


constants =
    { clickedPhoto = "clicked-photo" }


initialModel =
    { thumbnails =
        [ { fileName = "1.jpeg" }
        , { fileName = "2.jpeg" }
        , { fileName = "3.jpeg" }
        ]
    , selectedThumbnail = "1.jpeg"
    }


update msg model =
    if msg.action == constants.clickedPhoto then
        { model | selectedThumbnail = msg.target }

    else
        model


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
