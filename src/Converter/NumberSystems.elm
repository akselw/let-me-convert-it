module Converter.NumberSystems
    exposing
        ( convert
        , initRoman
        , initDecimal
        , addRomanDigit
        , addDecimalDigit
        , backspace
        , possibleNextRoman
        , possibleNextDecimal
        , parseTest
        , converter
        , input
        , inputName
        , outputName
        , selectInput
        , selectOutput
        , digitToString
        , romanDigitToString
        , decimalNumberToString
        , convertBinaryToInt
        )

import SelectList exposing (..)
import Converter.Types exposing (..)


converter : Converter
converter =
    NumberSystemConverter "Number systems"
        (SelectList.fromLists [] (RomanNumeralState initRoman) [ DecimalState initDecimal ])
        (SelectList.fromLists [ RomanNumeral ] Decimal [])


input : NumberSystemState -> String
input state =
    case state of
        RomanNumeralState number ->
            romanNumeralToString number

        DecimalState number ->
            decimalNumberToString number


inputName : NumberSystemState -> String
inputName state =
    case state of
        RomanNumeralState _ ->
            "Roman numerals"

        DecimalState _ ->
            "Decimals"


outputName : NumberSystem -> String
outputName system =
    case system of
        RomanNumeral ->
            "Roman numerals"

        Decimal ->
            "Decimals"


decimalNumberToString : FourDigitDecimalNumber -> String
decimalNumberToString (FourDigitDecimalNumber numbers) =
    case numbers of
        ( Zero, Zero, Zero, Zero ) ->
            "0"

        ( Zero, Zero, Zero, ones ) ->
            digitToString ones

        ( Zero, Zero, tens, ones ) ->
            [ tens, ones ]
                |> List.map digitToString
                |> String.join ""

        ( Zero, hundreds, tens, ones ) ->
            [ hundreds, tens, ones ]
                |> List.map digitToString
                |> String.join ""

        ( thousands, hundreds, tens, ones ) ->
            [ thousands, hundreds, tens, ones ]
                |> List.map digitToString
                |> String.join ""


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


romanNumeralToString : RomanNumber -> String
romanNumeralToString (RomanNumber numbers) =
    numbers
        |> List.map romanDigitToString
        |> String.join ""


selectInput : String -> SelectList NumberSystemState -> SelectList NumberSystemState
selectInput name inputs =
    select (\input -> (inputName input) == name) inputs


selectOutput : String -> SelectList NumberSystem -> SelectList NumberSystem
selectOutput name outputs =
    select (\output -> (outputName output) == name) outputs


decimalGroup : RomanDigit -> DecimalGroup
decimalGroup digit =
    case digit of
        M ->
            Thousands

        D ->
            Hundreds

        C ->
            Hundreds

        L ->
            Tens

        X ->
            Tens

        V ->
            Ones

        I ->
            Ones


parseTest : RomanNumber -> GroupedRomanDigits
parseTest (RomanNumber digits) =
    parseRoman digits


parseRomanToList : List RomanDigit -> List ( DecimalGroup, RomanDigit )
parseRomanToList digits =
    case digits of
        a :: b :: rest ->
            if smallerRomanDigits a |> List.member b then
                ( decimalGroup a, a ) :: parseRomanToList (b :: rest)
            else
                ( decimalGroup a, a ) :: ( decimalGroup a, b ) :: parseRomanToList rest

        a :: [] ->
            [ ( decimalGroup a, a ) ]

        [] ->
            []


emptyGroupedRomans : GroupedRomanDigits
emptyGroupedRomans =
    { thousands = []
    , hundreds = []
    , tens = []
    , ones = []
    }


addToRomanGroup : ( DecimalGroup, RomanDigit ) -> GroupedRomanDigits -> GroupedRomanDigits
addToRomanGroup ( group, digit ) grouped =
    case group of
        Thousands ->
            { grouped | thousands = digit :: grouped.thousands }

        Hundreds ->
            { grouped | hundreds = digit :: grouped.hundreds }

        Tens ->
            { grouped | tens = digit :: grouped.tens }

        Ones ->
            { grouped | ones = digit :: grouped.ones }


parseRoman : List RomanDigit -> GroupedRomanDigits
parseRoman digits =
    parseRomanToList digits
        |> List.foldr addToRomanGroup emptyGroupedRomans


convert : NumberSystemState -> NumberSystem -> String
convert input output =
    case ( input, output ) of
        ( RomanNumeralState number, RomanNumeral ) ->
            romanNumeralToString number

        ( RomanNumeralState number, Decimal ) ->
            number
                |> convertFromRoman
                |> decimalNumberToString

        ( DecimalState number, RomanNumeral ) ->
            number
                |> convertToRoman
                |> romanNumeralToString

        ( DecimalState number, Decimal ) ->
            decimalNumberToString number


convertRomanDigitsToDigit : ( RomanDigit, RomanDigit, RomanDigit ) -> List RomanDigit -> DecimalDigit
convertRomanDigitsToDigit ( one, five, ten ) digits =
    if digits == [ one ] then
        One
    else if digits == [ one, one ] then
        Two
    else if digits == [ one, one, one ] then
        Three
    else if digits == [ one, five ] then
        Four
    else if digits == [ five ] then
        Five
    else if digits == [ five, one ] then
        Six
    else if digits == [ five, one, one ] then
        Seven
    else if digits == [ five, one, one, one ] then
        Eight
    else if digits == [ one, ten ] then
        Nine
    else
        Zero


convertRomanThousands : List RomanDigit -> DecimalDigit
convertRomanThousands romanDigits =
    if romanDigits == [ M ] then
        One
    else if romanDigits == [ M, M ] then
        Two
    else if romanDigits == [ M, M, M ] then
        Three
    else
        Zero


convertFromRoman : RomanNumber -> FourDigitDecimalNumber
convertFromRoman (RomanNumber digits) =
    let
        grouped =
            parseRoman digits
    in
        FourDigitDecimalNumber
            ( convertRomanThousands grouped.thousands
            , convertRomanDigitsToDigit ( C, D, M ) grouped.hundreds
            , convertRomanDigitsToDigit ( X, L, C ) grouped.tens
            , convertRomanDigitsToDigit ( I, V, X ) grouped.ones
            )


convertDigitToRomanDigit : ( RomanDigit, RomanDigit, RomanDigit ) -> DecimalDigit -> List RomanDigit
convertDigitToRomanDigit ( one, five, ten ) digit =
    case digit of
        One ->
            [ one ]

        Two ->
            [ one, one ]

        Three ->
            [ one, one, one ]

        Four ->
            [ one, five ]

        Five ->
            [ five ]

        Six ->
            [ five, one ]

        Seven ->
            [ five, one, one ]

        Eight ->
            [ five, one, one, one ]

        Nine ->
            [ one, ten ]

        Zero ->
            []


convertThousands : DecimalDigit -> List RomanDigit
convertThousands decimal =
    case decimal of
        One ->
            [ M ]

        Two ->
            [ M, M ]

        Three ->
            [ M, M, M ]

        _ ->
            []


convertToRoman : FourDigitDecimalNumber -> RomanNumber
convertToRoman (FourDigitDecimalNumber ( thousands, hundreds, tens, ones )) =
    (convertThousands thousands)
        ++ (convertDigitToRomanDigit ( C, D, M ) hundreds)
        ++ (convertDigitToRomanDigit ( X, L, C ) tens)
        ++ (convertDigitToRomanDigit ( I, V, X ) ones)
        |> RomanNumber


possibleNextDecimal : FourDigitDecimalNumber -> List DecimalDigit
possibleNextDecimal (FourDigitDecimalNumber ( thousands, hundreds, tens, ones )) =
    if thousands /= Zero || not (List.member hundreds [ Zero, One, Two, Three ]) then
        []
    else
        [ One
        , Two
        , Three
        , Four
        , Five
        , Six
        , Seven
        , Eight
        , Nine
        , Zero
        ]


addDecimalDigit : DecimalDigit -> FourDigitDecimalNumber -> FourDigitDecimalNumber
addDecimalDigit digit ((FourDigitDecimalNumber ( thousands, hundreds, tens, ones )) as numbers) =
    if possibleNextDecimal numbers |> List.member digit then
        FourDigitDecimalNumber ( hundreds, tens, ones, digit )
    else
        numbers


initDecimal : FourDigitDecimalNumber
initDecimal =
    FourDigitDecimalNumber ( Zero, Zero, Zero, Zero )


allRomanDigits : List RomanDigit
allRomanDigits =
    [ M
    , D
    , C
    , L
    , X
    , V
    , I
    ]


takeLast : Int -> List a -> List a
takeLast n list =
    list
        |> List.reverse
        |> List.take n
        |> List.reverse


inSizeGroup : RomanDigit -> RomanDigit -> Bool
inSizeGroup a b =
    let
        hundreds =
            [ M, D, C ]

        tens =
            [ C, L, X ]

        ones =
            [ X, V, I ]

        group x =
            case x of
                M ->
                    hundreds

                D ->
                    hundreds

                C ->
                    hundreds ++ tens

                L ->
                    tens

                X ->
                    tens ++ ones

                V ->
                    ones

                I ->
                    ones
    in
        group a
            |> List.member b


possibleSingle : RomanDigit -> List RomanDigit
possibleSingle digit =
    case digit of
        I ->
            [ I, V, X ]

        V ->
            [ I ]

        X ->
            [ I, V, X, L, C ]

        L ->
            [ I, V, X ]

        C ->
            allRomanDigits

        D ->
            [ I, V, X, L, C ]

        M ->
            allRomanDigits


takeAfter : (a -> Bool) -> List a -> List a
takeAfter f list =
    case list of
        [] ->
            []

        x :: xs ->
            if f x then
                xs
            else
                takeAfter f xs


smallerRomanDigits : RomanDigit -> List RomanDigit
smallerRomanDigits d =
    takeAfter ((==) d) [ M, D, C, L, X, V, I ]


unaryRomanDigit : RomanDigit -> Bool
unaryRomanDigit d =
    List.member d [ I, X, C, M ]


smallerThan : RomanDigit -> RomanDigit -> Bool
smallerThan a b =
    smallerRomanDigits b |> List.member a


oneLarger : RomanDigit -> Maybe RomanDigit
oneLarger digit =
    case digit of
        I ->
            Just V

        V ->
            Just X

        X ->
            Just L

        L ->
            Just C

        C ->
            Just D

        D ->
            Just M

        M ->
            Nothing


isOneLarger : RomanDigit -> RomanDigit -> Bool
isOneLarger possiblyOneLarger a =
    case oneLarger a of
        Just n ->
            n == possiblyOneLarger

        Nothing ->
            False


possibleDouble : RomanDigit -> RomanDigit -> List RomanDigit
possibleDouble first second =
    if first == second then
        first :: smallerRomanDigits first
    else if smallerThan first second then
        smallerRomanDigits first
    else if not (unaryRomanDigit second) then
        smallerRomanDigits second
    else if isOneLarger first second then
        -- DC, LX, VI
        smallerRomanDigits first
    else
        possibleSingle second


possibleTriple : RomanDigit -> RomanDigit -> RomanDigit -> List RomanDigit
possibleTriple a b c =
    if a == b && a == c then
        smallerRomanDigits c
    else if smallerThan b a then
        possibleDouble b c
    else
        possibleSingle c


possibleNextRoman : RomanNumber -> List RomanDigit
possibleNextRoman (RomanNumber digits) =
    let
        last =
            takeLast 3 digits
    in
        case last of
            [] ->
                allRomanDigits

            a :: [] ->
                possibleSingle a

            a :: b :: [] ->
                possibleDouble a b

            a :: b :: c :: _ ->
                possibleTriple a b c


addRomanDigit : RomanDigit -> RomanNumber -> RomanNumber
addRomanDigit digit ((RomanNumber digits) as numbers) =
    if possibleNextRoman numbers |> List.member digit then
        RomanNumber (digits ++ [ digit ])
    else
        numbers


backspaceDecimal : FourDigitDecimalNumber -> FourDigitDecimalNumber
backspaceDecimal (FourDigitDecimalNumber ( thousands, hundreds, tens, ones )) =
    FourDigitDecimalNumber ( Zero, thousands, hundreds, tens )


backspaceRoman : RomanNumber -> RomanNumber
backspaceRoman (RomanNumber numbers) =
    numbers
        |> List.take ((List.length numbers) - 1)
        |> RomanNumber


backspace : NumberSystemState -> NumberSystemState
backspace state =
    case state of
        RomanNumeralState number ->
            number
                |> backspaceRoman
                |> RomanNumeralState

        DecimalState number ->
            number
                |> backspaceDecimal
                |> DecimalState


initRoman : RomanNumber
initRoman =
    RomanNumber []


convertBinaryToInt : BinaryNumber -> Int
convertBinaryToInt (BinaryNumber digits) =
    let
        helper : List BinaryDigit -> Int
        helper digits =
            case digits of
                [] ->
                    0

                a :: rest ->
                    case a of
                        BinaryZero ->
                            helper rest

                        BinaryOne ->
                            (2 ^ (List.length rest)) + helper rest
    in
        helper digits
