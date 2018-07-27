module Converter.Value exposing (..)

import Converter.Digits exposing (..)


type CommaOrMinor
    = Comma Bool
    | Minor String


type Value
    = IntValue String Bool
    | CommaOrMinorValue CommaOrMinor
    | DecimalValue DecimalDigit Bool
    | RomanValue RomanDigit Bool
