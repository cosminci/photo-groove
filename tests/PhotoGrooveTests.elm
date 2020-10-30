module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decoder
import Json.Encode as Encoder
import PhotoGroove exposing (Message(..), initialModel, thumbnailDecoder, update)
import Test exposing (..)


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


slidHueSetsHue : Test
slidHueSetsHue =
    fuzz int "SlidHue sets the hue" <|
        \amount ->
            initialModel
                |> update (HueFilterUpdated amount)
                |> Tuple.first
                |> .hue
                |> Expect.equal amount
