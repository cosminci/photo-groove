module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Attributes as Attr exposing (src)
import Json.Decode as Decoder
import Json.Encode as Encoder
import List exposing (all)
import PhotoGroove exposing (Message(..), Model, State(..), Thumbnail, initialModel, thumbnailDecoder, update, urlPrefix, view)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag, text)


thumbnailDecoderTest : Test
thumbnailDecoderTest =
    fuzz2 string int "title should default to 'untitled'" <|
        \fileName size ->
            [ ( "url", Encoder.string fileName )
            , ( "size", Encoder.int size )
            ]
                |> Encoder.object
                |> Decoder.decodeValue thumbnailDecoder
                |> Result.map .title
                |> Expect.equal
                    (Ok "untitled")


slidersTest : Test
slidersTest =
    describe "Slider updates correctly reflect in the model"
        [ sliderTest "Hue" HueFilterUpdated .hue
        , sliderTest "Ripple" RippleFilterUpdated .ripple
        , sliderTest "Noise" NoiseFilterUpdated .noise
        ]


sliderTest : String -> (Int -> Message) -> (Model -> Int) -> Test
sliderTest description toMessage amountFromModel =
    fuzz int description <|
        \amount ->
            initialModel
                |> update (toMessage amount)
                |> Tuple.first
                |> amountFromModel
                |> Expect.equal amount


noPhotosNoThumbnails : Test
noPhotosNoThumbnails =
    test "No thumbnails render when there are no photos to render." <|
        \_ ->
            initialModel
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)


thumbnailsWork : Test
thumbnailsWork =
    fuzz urlFuzzer "URLs render as thumbnails" <|
        \urls ->
            { initialModel | state = Loaded (List.map photoFromUrl urls) (photoFromUrl "") }
                |> view
                |> Query.fromHtml
                |> Expect.all (List.map thumbnailRendered urls)


photoFromUrl : String -> Thumbnail
photoFromUrl url =
    { fileName = url, size = 0, title = "" }


thumbnailRendered : String -> Query.Single msg -> Expectation
thumbnailRendered url parentDiv =
    parentDiv
        |> Query.findAll [ tag "img", attribute (Attr.src (urlPrefix ++ url)) ]
        |> Query.count (Expect.atLeast 1)


urlFuzzer : Fuzzer (List String)
urlFuzzer =
    Fuzz.intRange 1 5
        |> Fuzz.map urlsFromCount


urlsFromCount : Int -> List String
urlsFromCount urlCount =
    List.range 1 urlCount
        |> List.map (\num -> String.fromInt num ++ ".png")


clickThumbnail : Test
clickThumbnail =
    fuzz3 urlFuzzer string urlFuzzer "clicking a thumbnail selects it" <|
        \urlsBefore urlToSelect urlsAfter ->
            let
                url =
                    urlToSelect ++ ".jpeg"

                photos =
                    (urlsBefore ++ url :: urlsAfter) |> List.map photoFromUrl

                srcToClick =
                    urlPrefix ++ url
            in
            { initialModel | state = Loaded photos (photoFromUrl "") }
                |> view
                |> Query.fromHtml
                |> Query.find [ tag "img", attribute (Attr.src srcToClick) ]
                |> Event.simulate Event.click
                |> Event.expect (ThumbnailClicked (photoFromUrl url))
