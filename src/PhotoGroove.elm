module PhotoGroove exposing (Message, main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick)
import Http
import Random exposing (Generator, generate, uniform)


type alias UrlPrefix =
    String


urlPrefix : UrlPrefix
urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail : Thumbnail -> Thumbnail -> Html Message
viewThumbnail selectedThumbnail thumbnail =
    img
        [ src <| urlPrefix ++ thumbnail.fileName
        , classList [ ( "selected", selectedThumbnail == thumbnail ) ]
        , onClick <| ThumbnailClicked thumbnail
        ]
        []


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Message
viewSizeChooser selectedSize size =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , onCheck <|
                \checked ->
                    if checked then
                        SizeChanged size

                    else
                        SizeChanged selectedSize
            , checked <| selectedSize == size
            ]
            []
        , text <| showSize size
        ]


showSize : ThumbnailSize -> String
showSize size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


viewLoaded : List Thumbnail -> Thumbnail -> ThumbnailSize -> List (Html Message)
viewLoaded thumbnails selected size =
    [ h1 [] [ text "Photo Groove" ]
    , button [ onClick SurpriseMeClicked ] [ text "Surprise Me!" ]
    , div [ id "choose-size" ]
        (List.map (viewSizeChooser size) [ Small, Medium, Large ])
    , div [ id "thumbnails", class <| showSize size ]
        (List.map (viewThumbnail selected) thumbnails)
    , img [ class "large", src <| urlPrefix ++ "large/" ++ selected.fileName ] []
    ]


view : Model -> Html Message
view model =
    div [ class "content" ] <|
        case model.state of
            Loaded thumbnails selected ->
                viewLoaded thumbnails selected model.size

            Loading ->
                []

            Errored error ->
                [ text <| "Error" ++ error ]


type alias FileName =
    String


type alias Thumbnail =
    { fileName : FileName }


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Model =
    { state : State, size : ThumbnailSize }


type Message
    = ThumbnailClicked Thumbnail
    | SurpriseMeClicked
    | SizeChanged ThumbnailSize
    | ThumbnailRandomlyPicked Thumbnail
    | LoadedThumbnails (Result Http.Error String)


type State
    = Loading
    | Loaded (List Thumbnail) Thumbnail
    | Errored String


initialModel : Model
initialModel =
    { state = Loading
    , size = Large
    }


handleLoadedThumbnails : Result Http.Error String -> Model -> Model
handleLoadedThumbnails result model =
    case result of
        Ok responseStr ->
            let
                fileNames =
                    String.split "," responseStr

                thumbnails =
                    List.map Thumbnail fileNames
            in
            case thumbnails of
                firstThumbnail :: _ ->
                    { model | state = Loaded thumbnails firstThumbnail }

                [] ->
                    { model | state = Errored "No thumbnails loaded" }

        Err _ ->
            { model | state = Errored "HTTP error" }


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case ( message, model.state ) of
        ( SizeChanged size, _ ) ->
            ( { model | size = size }, Cmd.none )

        ( LoadedThumbnails result, _ ) ->
            ( handleLoadedThumbnails result model, Cmd.none )

        ( ThumbnailClicked thumbnail, Loaded (firstThumbnail :: otherThumbnails) selected ) ->
            ( { model | state = Loaded (firstThumbnail :: otherThumbnails) selected }, Cmd.none )

        ( SurpriseMeClicked, Loaded ((firstThumbnail :: otherThumbnails) as thumbnails) selected ) ->
            Random.uniform firstThumbnail thumbnails
                |> Random.generate ThumbnailRandomlyPicked
                |> Tuple.pair model

        ( ThumbnailRandomlyPicked thumbnail, Loaded thumbnails selected ) ->
            ( { model | state = Loaded thumbnails thumbnail }, Cmd.none )

        _ ->
            ( model, Cmd.none )


initialCmd : Cmd Message
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list"
        , expect = Http.expectString (\result -> LoadedThumbnails result)
        }


main : Program () Model Message
main =
    Browser.element
        { init = \flags -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
