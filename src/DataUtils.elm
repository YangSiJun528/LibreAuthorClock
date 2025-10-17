module DataUtils exposing (parseQuote, parseJSON, parseTextWithBold, loadResource)

import Data exposing (QuoteData, TextFragment(..))
import Dict exposing (Dict)
import Json.Decode as Decode
import Http
import Regex exposing (Regex)
import Regex

-- PARSE Quote file format

parseQuote : String -> Dict String (List QuoteData)
parseQuote content =
    content
        |> String.split "\n"
        |> List.filterMap parseLine
        |> List.foldl groupByTimeKey Dict.empty

parseLine : String -> Maybe ( String, QuoteData )
parseLine line =
    case String.split "|=|" line of
        [ timeKey, timeDisplay, quoteText, quoteSource, quoteAuthor ] ->
            Just
                ( timeKey
                , { timeDisplay = timeDisplay
                  , quoteText = parseTextWithBold timeDisplay quoteText
                  , quoteSource = quoteSource
                  , quoteAuthor = quoteAuthor
                  }
                )

        _ ->
            Nothing

parseTextWithBold : String -> String -> List TextFragment
parseTextWithBold timeDisplay text =
    let
        escaped = timeDisplay
        pattern = Maybe.withDefault Regex.never (Regex.fromString ("(?i)" ++ escaped))
        matches = Regex.find pattern text
    in
    case matches of
        [] ->
            [ Normal text ]

        _ ->
            buildFragments text matches 0 []

buildFragments : String -> List Regex.Match -> Int -> List TextFragment -> List TextFragment
buildFragments text matches currentIndex acc =
    case matches of
        [] ->
            let
                remaining = String.dropLeft currentIndex text
            in
            if String.isEmpty remaining then
                List.reverse acc
            else
                List.reverse (Normal remaining :: acc)

        match :: rest ->
            let
                beforeMatch = String.slice currentIndex match.index text
                newAcc =
                    if String.isEmpty beforeMatch then
                        Bold match.match :: acc
                    else
                        Bold match.match :: Normal beforeMatch :: acc
                nextIndex = match.index + String.length match.match
            in
            buildFragments text rest nextIndex newAcc

groupByTimeKey : ( String, QuoteData ) -> Dict String (List QuoteData) -> Dict String (List QuoteData)
groupByTimeKey ( timeKey, quoteData ) dict =
    Dict.update timeKey
        (\maybeList ->
            case maybeList of
                Just list ->
                    Just (quoteData :: list)

                Nothing ->
                    Just [ quoteData ]
        )
        dict


-- PARSE JSON (simplified - assumes valid JSON structure)

parseJSON : String -> Dict String String
parseJSON content =
    case Decode.decodeString (Decode.dict Decode.string) content of
        Ok dict ->
            dict

        Err _ ->
            Dict.empty


-- LOAD FILE DATA

loadResource : String -> (Result Http.Error String -> msg) -> Cmd msg
loadResource url toMsg =
    Http.get
        { url = url
        , expect = Http.expectString toMsg
        }
