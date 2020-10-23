module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick)
import Random exposing (Generator, generate)


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
        , onClick (ThumbnailClicked thumbnail)
        ]
        []


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Message
viewSizeChooser selectedSize size =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , onCheck (sizeChanged size)
            , checked (selectedSize == size)
            ]
            []
        , text (showSize size)
        ]


sizeChanged : ThumbnailSize -> Bool -> Message
sizeChanged size checked =
    if checked == True then
        SizeChanged size

    else
        Noop


showSize : ThumbnailSize -> String
showSize size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


view : Model -> Html Message
view model =
    div [ class "context" ]
        [ h1 [] [ text "Photo Groove" ]
        , button [ onClick SurpriseMeClicked ] [ text "Surprise Me!" ]
        , div [ id "choose-size" ]
            (List.map (viewSizeChooser model.size) [ Small, Medium, Large ])
        , div [ id "thumbnails", class (showSize model.size) ]
            (List.map (viewThumbnail model.selected) model.thumbnails)
        , img [ class "large", src (urlPrefix ++ "large/" ++ model.selected.fileName) ] []
        ]


type alias FileName =
    String


type alias Thumbnail =
    { fileName : FileName }


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Model =
    { thumbnails : List Thumbnail, selected : Thumbnail, size : ThumbnailSize }


initialModel : Model
initialModel =
    { thumbnails =
        [ { fileName = "1.jpeg" }
        , { fileName = "2.jpeg" }
        , { fileName = "3.jpeg" }
        ]
    , selected = { fileName = "1.jpeg" }
    , size = Large
    }


randomThumbnailSelector : Random.Generator Int
randomThumbnailSelector =
    Random.int 0 (Array.length thumbnailsArray - 1)


thumbnailsArray : Array Thumbnail
thumbnailsArray =
    Array.fromList initialModel.thumbnails


type Message
    = ThumbnailClicked Thumbnail
    | SurpriseMeClicked
    | SizeChanged ThumbnailSize
    | ThumbnailIndexRandomlySelected Int
    | Noop


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        ThumbnailClicked thumbnail ->
            ( { model | selected = thumbnail }, Cmd.none )

        SurpriseMeClicked ->
            ( model, Random.generate ThumbnailIndexRandomlySelected randomThumbnailSelector )

        ThumbnailIndexRandomlySelected index ->
            ( { model | selected = Maybe.withDefault model.selected (Array.get index thumbnailsArray) }, Cmd.none )

        SizeChanged size ->
            ( { model | size = size }, Cmd.none )

        Noop ->
            ( model, Cmd.none )


main : Program () Model Message
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
