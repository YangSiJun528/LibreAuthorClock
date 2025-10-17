module Data exposing (TextFragment(..), QuoteData)

type TextFragment
    = Normal String
    | Bold String

type alias QuoteData =
    { timeDisplay : String
    , quoteText : List TextFragment
    , quoteSource : String
    , quoteAuthor : String
    }

