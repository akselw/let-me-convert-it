module Converter.Digits exposing (..)

-- Number Systems


type RomanDigit
    = M
    | D
    | C
    | L
    | X
    | V
    | I


type DecimalDigit
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Zero


type BinaryDigit
    = BinaryZero
    | BinaryOne


type HexDigit
    = HexOne
    | HexTwo
    | HexThree
    | HexFour
    | HexFive
    | HexSix
    | HexSeven
    | HexEight
    | HexNine
    | HexTen
    | HexEleven
    | HexTwelve
    | HexThirteen
    | HexFourteen
    | HexFifteen
    | HexZero


digitToString : DecimalDigit -> String
digitToString digit =
    case digit of
        One ->
            "1"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Zero ->
            "0"


romanDigitToString : RomanDigit -> String
romanDigitToString digit =
    case digit of
        M ->
            "M"

        D ->
            "D"

        C ->
            "C"

        L ->
            "L"

        X ->
            "X"

        V ->
            "V"

        I ->
            "I"


binaryDigitToString : BinaryDigit -> String
binaryDigitToString digit =
    case digit of
        BinaryZero ->
            "0"

        BinaryOne ->
            "1"


hexDigitToString : HexDigit -> String
hexDigitToString digit =
    case digit of
        HexOne ->
            "1"

        HexTwo ->
            "2"

        HexThree ->
            "3"

        HexFour ->
            "4"

        HexFive ->
            "5"

        HexSix ->
            "6"

        HexSeven ->
            "7"

        HexEight ->
            "8"

        HexNine ->
            "9"

        HexTen ->
            "A"

        HexEleven ->
            "B"

        HexTwelve ->
            "C"

        HexThirteen ->
            "D"

        HexFourteen ->
            "E"

        HexFifteen ->
            "F"

        HexZero ->
            "0"
