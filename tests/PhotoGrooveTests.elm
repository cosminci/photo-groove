module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decoder exposing (decodeString)
import PhotoGroove
import Test exposing (..)


thumbnailDecoderTest : Test
thumbnailDecoderTest =
    test "title defaults to (untitled)"
        (\_ ->
            """
            {"url": "fruits.com", "size": 5}
            """
                |> decodeString PhotoGroove.thumbnailDecoder
                |> Expect.equal
                    (Ok { fileName = "fruits.com", size = 5, title = "untitled" })
        )
