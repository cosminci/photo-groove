module PhotoGroove exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias UrlPrefix =
    String


urlPrefix : UrlPrefix
urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail : Thumbnail -> Thumbnail -> Html Message
viewThumbnail selectedThumbnail thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.fileName)
        , classList [ ( "selected", selectedThumbnail == thumbnail ) ]
        , onClick { action = actions.thumbnailClicked, target = thumbnail }
        ]
        []


view : Model -> Html Message
view model =
    div [ class "context" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            (List.map (viewThumbnail model.selectedThumbnail)
                model.thumbnails
            )
        , img [ class "large", src (urlPrefix ++ "large/" ++ model.selectedThumbnail.fileName) ] []
        ]


type alias Action =
    String


type alias Actions =
    { thumbnailClicked : Action }


actions : Actions
actions =
    { thumbnailClicked = "thumbnail-clicked" }


type alias FileName =
    String


type alias Thumbnail =
    { fileName : FileName }


type alias Model =
    { thumbnails : List Thumbnail, selectedThumbnail : Thumbnail }


initialModel : Model
initialModel =
    { thumbnails =
        [ { fileName = "1.jpeg" }
        , { fileName = "2.jpeg" }
        , { fileName = "3.jpeg" }
        ]
    , selectedThumbnail = { fileName = "1.jpeg" }
    }


type alias Message =
    { action : Action, target : Thumbnail }


update : Message -> Model -> Model
update msg model =
    if msg.action == actions.thumbnailClicked then
        { model | selectedThumbnail = msg.target }

    else
        model


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
