module PhotoGroove exposing (Message, main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onCheck, onClick)
import Http
import Json.Decode as Decoder exposing (..)
import Json.Decode.Pipeline as Decoder exposing (optional, required)
import Json.Encode as Encoder exposing (int)
import Random exposing (Generator, generate, uniform)


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail : Thumbnail -> Thumbnail -> Html Message
viewThumbnail selectedThumbnail thumbnail =
    img
        [ src <| urlPrefix ++ thumbnail.fileName
        , title (thumbnail.title ++ " [" ++ String.fromInt thumbnail.size ++ " KB]")
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
            , onCheck <|
                \checked ->
                    if checked then
                        SizeChanged size

                    else
                        SizeChanged selectedSize
            , checked (selectedSize == size)
            ]
            []
        , text (showSize size)
        ]


onSlide : (Int -> Message) -> Attribute Message
onSlide slideEventMapper =
    Decoder.at [ "detail", "slidTo" ] Decoder.int
        |> Decoder.map slideEventMapper
        |> on "slide"


viewFilter : (Int -> Message) -> String -> Int -> Html Message
viewFilter slideEventMapper name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (Encoder.int magnitude)
            , onSlide slideEventMapper
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
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


viewLoaded : List Thumbnail -> Thumbnail -> Model -> List (Html Message)
viewLoaded thumbnails selected model =
    [ h1 [] [ text "Photo Groove" ]
    , button [ onClick SurpriseMeClicked ] [ text "Surprise Me!" ]
    , div [ class "filters" ]
        [ viewFilter HueFilterUpdated "Hue" model.hue
        , viewFilter RippleFilterUpdated "Ripple" model.ripple
        , viewFilter NoiseFilterUpdated "Noise" model.noise
        ]
    , div [ id "choose-size" ]
        (List.map (viewSizeChooser model.size) [ Small, Medium, Large ])
    , div [ id "thumbnails", class <| showSize model.size ]
        (List.map (viewThumbnail selected) thumbnails)
    , img [ class "large", src <| urlPrefix ++ "large/" ++ selected.fileName ] []
    ]


view : Model -> Html Message
view model =
    div [ class "content" ] <|
        case model.state of
            Loaded thumbnails selected ->
                viewLoaded thumbnails selected model

            Loading ->
                []

            Errored error ->
                [ text <| "Error: " ++ error ]


type alias Thumbnail =
    { fileName : String, size : Int, title : String }


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Model =
    { state : State, size : ThumbnailSize, hue : Int, ripple : Int, noise : Int }


type Message
    = ThumbnailClicked Thumbnail
    | SurpriseMeClicked
    | SizeChanged ThumbnailSize
    | ThumbnailRandomlyPicked Thumbnail
    | LoadedThumbnails LoadThumbnailsResult
    | HueFilterUpdated Int
    | RippleFilterUpdated Int
    | NoiseFilterUpdated Int


type alias LoadThumbnailsResult =
    Result Http.Error (List Thumbnail)


type State
    = Loading
    | Loaded (List Thumbnail) Thumbnail
    | Errored String


initialModel : Model
initialModel =
    { state = Loading
    , size = Large
    , hue = 0
    , ripple = 0
    , noise = 0
    }


thumbnailDecoder : Decoder Thumbnail
thumbnailDecoder =
    succeed Thumbnail
        |> Decoder.required "url" Decoder.string
        |> Decoder.required "size" Decoder.int
        |> Decoder.optional "title" Decoder.string "untitled"


handleLoadedThumbnails : LoadThumbnailsResult -> Model -> Model
handleLoadedThumbnails result model =
    case result of
        Ok ((firstThumbnail :: _) as thumbnails) ->
            { model | state = Loaded thumbnails firstThumbnail }

        _ ->
            { model | state = Errored "Thumbnail loading failed" }


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case ( message, model.state ) of
        ( SizeChanged size, _ ) ->
            ( { model | size = size }, Cmd.none )

        ( LoadedThumbnails result, _ ) ->
            ( handleLoadedThumbnails result model, Cmd.none )

        ( ThumbnailClicked thumbnail, Loaded thumbnails selected ) ->
            ( { model | state = Loaded thumbnails thumbnail }, Cmd.none )

        ( SurpriseMeClicked, Loaded ((firstThumbnail :: otherThumbnails) as thumbnails) selected ) ->
            Random.uniform firstThumbnail thumbnails
                |> Random.generate ThumbnailRandomlyPicked
                |> Tuple.pair model

        ( ThumbnailRandomlyPicked thumbnail, Loaded thumbnails selected ) ->
            ( { model | state = Loaded thumbnails thumbnail }, Cmd.none )

        ( HueFilterUpdated newValue, _ ) ->
            ( { model | hue = newValue }, Cmd.none )

        ( RippleFilterUpdated newValue, _ ) ->
            ( { model | ripple = newValue }, Cmd.none )

        ( NoiseFilterUpdated newValue, _ ) ->
            ( { model | noise = newValue }, Cmd.none )

        _ ->
            ( model, Cmd.none )


initialCmd : Cmd Message
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson LoadedThumbnails (Decoder.list thumbnailDecoder)
        }


main : Program () Model Message
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children
