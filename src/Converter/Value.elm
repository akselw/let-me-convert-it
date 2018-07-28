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
    | BinaryValue BinaryDigit Bool
    | HexValue HexDigit Bool


toString : Value -> String
toString value =
    case value of
        IntValue s _ ->
            s

        CommaOrMinorValue e ->
            case e of
                Comma active ->
                    ","

                Minor minor ->
                    "+ " ++ minor

        DecimalValue digit _ ->
            digitToString digit

        RomanValue digit _ ->
            romanDigitToString digit

        BinaryValue digit _ ->
            binaryDigitToString digit

        HexValue digit _ ->
            hexDigitToString digit


isActive : Value -> Bool
isActive value =
    case value of
        IntValue _ active ->
            active

        CommaOrMinorValue e ->
            case e of
                Comma active ->
                    active

                Minor _ ->
                    True

        DecimalValue _ active ->
            active

        RomanValue _ active ->
            active

        BinaryValue _ active ->
            active

        HexValue _ active ->
            active
