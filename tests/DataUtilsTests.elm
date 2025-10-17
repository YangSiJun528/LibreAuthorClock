module DataUtilsTests exposing (..)

import Data exposing (TextFragment(..))
import Dict
import Expect
import DataUtils
import Test exposing (..)


suite : Test
suite =
    describe "DataUtils"
        [ describe "parser"
            [ test "parseDSV handles empty content" <|
                \() ->
                    DataUtils.parseQuote ""
                        |> Expect.equal Dict.empty
            , test "parseDSV handles lines that don't match format" <|
                \() ->
                    let
                        content = "this is a malformed line"
                    in
                    DataUtils.parseQuote content
                        |> Expect.equal Dict.empty
            , test "parseTextWithBold finds and bolds text" <|
                \() ->
                    let
                        text = "The time is 12:30."
                        timeDisplay = "12:30"
                        expected = [ Normal "The time is ", Bold "12:30", Normal "." ]
                    in
                        DataUtils.parseTextWithBold timeDisplay text
                        |> Expect.equal expected
            , test "parseDSV handles valid content" <|
                  \() ->
                      let
                          content = "00:02|=|00:02 AM|=|The time is 00:02 AM.|=|The Clock|=|The Author"
                          expected =
                              Dict.fromList
                                  [ ( "00:02"
                                    , [ { timeDisplay = "00:02 AM"
                                        , quoteText = [ Normal "The time is ", Bold "00:02 AM", Normal "." ]
                                        , quoteSource = "The Clock"
                                        , quoteAuthor = "The Author"
                                        }
                                      ]
                                    )
                                  ]
                      in
                      DataUtils.parseQuote content
                          |> Expect.equal expected
            ]
        ]
