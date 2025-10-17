module Main exposing (main)

import Browser
import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled exposing (Html, div, main_, option, select, span, strong, text, toUnstyled)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onInput)
import Http
import Json.Decode as Decode exposing (Decoder)
import Regex exposing (Regex)
import Task
import Time


-- MODEL

type TextFragment
    = Normal String
    | Bold String

type alias QuoteData =
    { timeDisplay : String
    , quoteText : List TextFragment
    , quoteSource : String
    , quoteAuthor : String
    }

type alias InternalState =
    { language : String
    , zone : Time.Zone
    , lastQuoteKey : String
    , allQuotes : Dict String (Dict String (List QuoteData))
    , allMessages : Dict String (Dict String String)
    }

type alias Model =
    { internal : InternalState
    , display : QuoteData
    }

initialModel : Model
initialModel =
    { internal =
        { language = "en"
        , zone = Time.utc
        , lastQuoteKey = ""
        , allQuotes = Dict.empty
        , allMessages = Dict.empty
        }
    , display =
        { timeDisplay = ""
        , quoteText = [ Normal "Loading..." ]
        , quoteSource = ""
        , quoteAuthor = ""
        }
    }


-- MESSAGES

type Msg
    = LanguageChanged String
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | QuotesLoaded String String
    | QuotesLoadFailed String
    | MessagesLoaded String String
    | MessagesLoadFailed String


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LanguageChanged lang ->
            let
                internal = model.internal
                newInternal =
                    { internal
                    | language = lang
                    , allQuotes = Dict.empty
                    , allMessages = Dict.empty
                    }
            in
            ( { model | internal = newInternal,  display = initialModel.display }
            , Cmd.batch
                [ loadQuotes lang
                , loadMessages lang
                ]
            )

        Tick newTime ->
            let
                internal = model.internal
                newQuoteKey = formatTimeKey internal.zone newTime
            in
            if newQuoteKey /= internal.lastQuoteKey then
                let
                    newInternal = { internal | lastQuoteKey = newQuoteKey }
                in
                ( updateDisplay { model | internal = newInternal }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        AdjustTimeZone newZone ->
            let
                internal = model.internal
                newInternal = { internal | zone = newZone }
            in
            ( { model | internal = newInternal }, Cmd.none )

        QuotesLoaded lang content ->
            let
                internal = model.internal
                parsedQuotes = parseDSV content
                newAllQuotes = Dict.insert lang parsedQuotes internal.allQuotes
                newInternal = { internal | allQuotes = newAllQuotes }
            in
            ( updateDisplay { model | internal = newInternal }
            , Cmd.none
            )

        QuotesLoadFailed lang ->
            let
                _ = Debug.log "Failed to load quotes for language" lang
            in
            ( model, Cmd.none )

        MessagesLoaded lang content ->
            let
                internal = model.internal
                parsedMessages = parseJSON content
                newAllMessages = Dict.insert lang parsedMessages internal.allMessages
                newInternal = { internal | allMessages = newAllMessages }
            in
            ( updateDisplay { model | internal = newInternal }
            , Cmd.none
            )

        MessagesLoadFailed lang ->
            let
                _ = Debug.log "Failed to load system messages for language" lang
            in
            ( model, Cmd.none )

-- PARSE DSV

parseDSV : String -> Dict String (List QuoteData)
parseDSV content =
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
        pattern = Maybe.withDefault Regex.never (Regex.fromString escaped)
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


-- HTTP

loadQuotes : String -> Cmd Msg
loadQuotes lang =
    Http.get
        { url = "i18n/" ++ lang ++ "/quote.dsv"
        , expect = Http.expectString (handleQuotesResponse lang)
        }

handleQuotesResponse : String -> Result Http.Error String -> Msg
handleQuotesResponse lang result =
    case result of
        Ok content ->
            QuotesLoaded lang content

        Err _ ->
            QuotesLoadFailed lang

loadMessages : String -> Cmd Msg
loadMessages lang =
    Http.get
        { url = "i18n/" ++ lang ++ "/system.json"
        , expect = Http.expectString (handleMessagesResponse lang)
        }

handleMessagesResponse : String -> Result Http.Error String -> Msg
handleMessagesResponse lang result =
    case result of
        Ok content ->
            MessagesLoaded lang content

        Err _ ->
            MessagesLoadFailed lang


-- DISPLAY UPDATE

updateDisplay : Model -> Model
updateDisplay model =
    let
        internal = model.internal
        quoteData = getQuote internal.language internal.lastQuoteKey internal.allQuotes
        messages = Dict.get internal.language internal.allMessages |> Maybe.withDefault Dict.empty

        noQuoteMsg =
            Dict.get "NO_QUOTE_FOUND" messages
                |> Maybe.withDefault "No quote found for this time."

        loadingMsg =
            Dict.get "QUOTE_LOADING" messages
                |> Maybe.withDefault "Loading quote..."
    in
    case quoteData of
        Just data ->
            { model | display = data }

        Nothing ->
            if Dict.isEmpty internal.allQuotes then
                { model
                    | display =
                        { timeDisplay = ""
                        , quoteText = [ Normal loadingMsg ]
                        , quoteSource = ""
                        , quoteAuthor = ""
                        }
                }
            else
                { model
                    | display =
                        { timeDisplay = ""
                        , quoteText = [ Normal noQuoteMsg ]
                        , quoteSource = ""
                        , quoteAuthor = ""
                        }
                }

formatTimeKey : Time.Zone -> Time.Posix -> String
formatTimeKey zone time =
    let
        hour = Time.toHour zone time |> String.fromInt |> String.padLeft 2 '0'
        minute = Time.toMinute zone time |> String.fromInt |> String.padLeft 2 '0'
    in
    hour ++ ":" ++ minute

getQuote : String -> String -> Dict String (Dict String (List QuoteData)) -> Maybe QuoteData
getQuote lang timeKey allQuotes =
    allQuotes
        |> Dict.get lang
        |> Maybe.andThen (Dict.get timeKey)
        |> Maybe.andThen List.head


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


-- STYLES

rootStyles : List Style
rootStyles =
    [ property "--size-xs" "0.25rem"
    , property "--size-sm" "0.5rem"
    , property "--size-md" "0.75rem"
    , property "--size-lg" "1rem"
    , property "--size-xl" "1.25rem"
    , property "--size-2xl" "1.5rem"
    , property "--size-3xl" "2rem"
    , property "--size-4xl" "2.5rem"
    , property "--size-5xl" "3rem"
    , property "--size-6xl" "4rem"
    , property "--font-size-xs" "0.75rem"
    , property "--font-size-sm" "0.875rem"
    , property "--font-size-md" "1rem"
    , property "--font-size-lg" "1.125rem"
    , property "--font-size-xl" "1.3125rem"
    , property "--font-size-2xl" "1.5rem"
    , property "--font-size-3xl" "2.625rem"
    , property "--font-size-4xl" "4rem"
    , property "--font-weight-light" "300"
    , property "--font-weight-normal" "400"
    , property "--font-weight-bold" "700"
    , property "--color-text" "#111"
    , property "--color-background" "#FBFBFB"
    , property "--color-border" "#DDD"
    , fontFamilies [ "Georgia", "Times New Roman", "Times", "serif" ]
    , backgroundColor (hex "FBFBFB")
    , color (hex "111")
    , margin zero
    , displayFlex
    , flexDirection column
    , justifyContent center
    , alignItems center
    , minHeight (vh 100)
    ]

langSelectorContainerStyles : List Style
langSelectorContainerStyles =
    [ position absolute
    , property "top" "var(--size-3xl)"
    , property "right" "var(--size-3xl)"
    ]

langSelectorStyles : List Style
langSelectorStyles =
    [ fontFamilies [ "inherit" ]
    , property "font-size" "var(--font-size-md)"
    , property "padding" "var(--size-sm)"
    , border3 (px 1) solid (hex "DDD")
    , property "border-radius" "var(--size-xs)"
    , backgroundColor (hex "FBFBFB")
    , color (hex "111")
    ]

mainStyles : List Style
mainStyles =
    [ textAlign center
    , property "padding" "var(--size-3xl)"
    , maxWidth (px 700)
    ]

sourceContainerStyles: List Style
sourceContainerStyles =
    [ property "text-align" "right"
    ]

quoteStyles : List Style
quoteStyles =
    [ property "font-size" "var(--font-size-3xl)"
    , property "font-weight" "var(--font-weight-light)"
    , property "margin-bottom" "var(--size-2xl)"
    ]

displayTimeStyles : List Style
displayTimeStyles =
    [ property "font-weight" "var(--font-weight-bold)"
    ]

titleStyles : List Style
titleStyles =
    [ fontStyle italic
    , property "font-size" "var(--font-size-xl)"
    ]

authorStyles : List Style
authorStyles =
    [ property "font-size" "var(--font-size-md)"
    ]


-- VIEW

renderTextFragments : List TextFragment -> List (Html msg)
renderTextFragments fragments =
    List.map renderFragment fragments

renderFragment : TextFragment -> Html msg
renderFragment fragment =
    case fragment of
        Normal str ->
            span [] [ text str ]

        Bold str ->
            strong [ css displayTimeStyles ] [ text str ]

view : Model -> Html Msg
view model =
    let
        display = model.display
        lang = model.internal.language
    in
    div [ css rootStyles ]
        [ div [ css langSelectorContainerStyles ]
            [ select [ css langSelectorStyles, onInput LanguageChanged, value lang ]
                [ option [ value "en" ] [ text "English" ]
                , option [ value "ko" ] [ text "한국어(Korean)" ]
                ]
            ]
        , main_ [ css mainStyles ]
            [ div [ css quoteStyles ] (renderTextFragments display.quoteText)
            , div [ css sourceContainerStyles ]
                [ div [ css titleStyles ] [ text display.quoteSource ]
                , div [ css authorStyles ] [ text display.quoteAuthor ]
                ]
            ]
        ]


-- INIT

init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , loadQuotes "en"
        , loadMessages "en"
        ]
    )


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }
