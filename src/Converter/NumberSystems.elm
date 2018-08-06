module Converter.NumberSystems
    exposing
        ( NumberSystem
          -- input/output
        , input
        , addToInput
        , backspace
        , convert
          -- names
        , converterName
        , inputName
        , outputName
        , mapInputNames
        , mapOutputNames
          -- selection
        , selectInput
        , selectOutput
          -- values
        , values
          -- converters
        , converter
        )

import SelectList exposing (..)
import Common exposing (mapSelected)
import Converter.Digits exposing (..)
import Converter.Fields exposing (..)
import Converter.Value exposing (..)
import Converter.Values exposing (..)


-- Types


type NumberSystem
    = NumberSystem String (SelectList NumberSystemOutput) (SelectList NumberSystemState)


type NumberSystemOutput
    = RomanNumeral
    | Decimal
    | Binary
    | Hex


type NumberSystemState
    = RomanNumeralState RomanNumber
    | DecimalState DecimalNumber
    | BinaryState BinaryNumber
    | HexState HexNumber


type DecimalGroup
    = Thousands
    | Hundreds
    | Tens
    | Ones


type alias GroupedRomanDigits =
    { thousands : List RomanDigit
    , hundreds : List RomanDigit
    , tens : List RomanDigit
    , ones : List RomanDigit
    }


type FourDigitDecimalNumber
    = FourDigitDecimalNumber ( DecimalDigit, DecimalDigit, DecimalDigit, DecimalDigit )


type DecimalNumber
    = DecimalNumber (List DecimalDigit)


type RomanNumber
    = RomanNumber (List RomanDigit)


type BinaryNumber
    = BinaryNumber (List BinaryDigit)


type HexNumber
    = HexNumber (List HexDigit)



-- Functions


converter : NumberSystem
converter =
    NumberSystem "Number systems"
        (SelectList.fromLists [ RomanNumeral ] Decimal [ Binary, Hex ])
        (SelectList.fromLists []
            (RomanNumeralState initRoman)
            [ DecimalState initDecimal
            , BinaryState initBinary
            , HexState initHex
            ]
        )


converterName : NumberSystem -> String
converterName (NumberSystem name _ _) =
    name


getInput : NumberSystemState -> String
getInput state =
    case state of
        RomanNumeralState number ->
            romanNumeralToString number

        DecimalState number ->
            decimalNumberToString number

        BinaryState number ->
            binaryNumberToString number

        HexState number ->
            hexNumberToString number


input : NumberSystem -> InputField
input (NumberSystem _ _ inputs) =
    inputs
        |> selected
        |> getInput
        |> SingleStringInputField


inputSystemName : NumberSystemState -> String
inputSystemName state =
    case state of
        RomanNumeralState _ ->
            "Roman numerals"

        DecimalState _ ->
            "Decimals"

        BinaryState _ ->
            "Binary"

        HexState _ ->
            "Hexadecimal"


inputName : NumberSystem -> String
inputName (NumberSystem _ _ inputs) =
    inputs
        |> selected
        |> inputSystemName


outputSystemName : NumberSystemOutput -> String
outputSystemName system =
    case system of
        RomanNumeral ->
            "Roman numerals"

        Decimal ->
            "Decimals"

        Binary ->
            "Binary"

        Hex ->
            "Hexadecimal"


outputName : NumberSystem -> String
outputName (NumberSystem _ outputs _) =
    outputs
        |> selected
        |> outputSystemName


fourDigitDecimalNumberToString : FourDigitDecimalNumber -> String
fourDigitDecimalNumberToString (FourDigitDecimalNumber numbers) =
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


binaryNumberToString : BinaryNumber -> String
binaryNumberToString (BinaryNumber number) =
    number
        |> List.map binaryDigitToString
        |> String.join ""


decimalNumberToString : DecimalNumber -> String
decimalNumberToString (DecimalNumber number) =
    number
        |> List.map digitToString
        |> String.join ""


romanNumeralToString : RomanNumber -> String
romanNumeralToString (RomanNumber numbers) =
    numbers
        |> List.map romanDigitToString
        |> String.join ""


hexNumberToString : HexNumber -> String
hexNumberToString (HexNumber numbers) =
    numbers
        |> List.map hexDigitToString
        |> String.join ""


selectInputF : String -> SelectList NumberSystemState -> SelectList NumberSystemState
selectInputF name inputs =
    select (inputSystemName >> (==) name) inputs


selectInput : String -> NumberSystem -> NumberSystem
selectInput name (NumberSystem converterName outputs inputs) =
    inputs
        |> selectInputF name
        |> NumberSystem converterName outputs


selectOutputF : String -> SelectList NumberSystemOutput -> SelectList NumberSystemOutput
selectOutputF name outputs =
    select (\output -> (outputSystemName output) == name) outputs


selectOutput : String -> NumberSystem -> NumberSystem
selectOutput name (NumberSystem converterName outputs inputs) =
    NumberSystem converterName (selectOutputF name outputs) inputs


romanValue : RomanNumber -> RomanDigit -> Value
romanValue number digit =
    number
        |> possibleNextRoman
        |> List.member digit
        |> RomanValue digit


decimalValue : DecimalNumber -> NumberSystemOutput -> DecimalDigit -> Value
decimalValue number output digit =
    number
        |> possibleNextDecimal output
        |> List.member digit
        |> DecimalValue digit


values : NumberSystem -> Values
values (NumberSystem _ outputs inputs) =
    case (selected inputs) of
        DecimalState number ->
            let
                val =
                    outputs
                        |> selected
                        |> decimalValue number
            in
                IntValues
                    { zero = val Zero
                    , one = val One
                    , two = val Two
                    , three = val Three
                    , four = val Four
                    , five = val Five
                    , six = val Six
                    , seven = val Seven
                    , eight = val Eight
                    , nine = val Nine
                    }

        RomanNumeralState number ->
            RomanValues
                { m = romanValue number M
                , d = romanValue number D
                , c = romanValue number C
                , l = romanValue number L
                , x = romanValue number X
                , v = romanValue number V
                , i = romanValue number I
                }

        BinaryState _ ->
            BinaryValues
                { zero = BinaryValue BinaryZero True
                , one = BinaryValue BinaryOne True
                }

        HexState _ ->
            HexValues
                { zero = HexValue HexZero True
                , one = HexValue HexOne True
                , two = HexValue HexTwo True
                , three = HexValue HexThree True
                , four = HexValue HexFour True
                , five = HexValue HexFive True
                , six = HexValue HexSix True
                , seven = HexValue HexSeven True
                , eight = HexValue HexEight True
                , nine = HexValue HexNine True
                , ten = HexValue HexTen True
                , eleven = HexValue HexEleven True
                , twelve = HexValue HexTwelve True
                , thirteen = HexValue HexThirteen True
                , fourteen = HexValue HexFourteen True
                , fifteen = HexValue HexFifteen True
                }


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


addValueToNumberSystemInput : NumberSystemOutput -> Value -> NumberSystemState -> NumberSystemState
addValueToNumberSystemInput output value input =
    case ( input, value ) of
        ( DecimalState number, DecimalValue digit _ ) ->
            number
                |> addDecimalDigit output digit
                |> DecimalState

        ( RomanNumeralState number, RomanValue digit _ ) ->
            number
                |> addRomanDigit digit
                |> RomanNumeralState

        ( BinaryState number, BinaryValue digit _ ) ->
            number
                |> addBinaryDigit digit
                |> BinaryState

        ( HexState number, HexValue digit _ ) ->
            number
                |> addHexDigit digit
                |> HexState

        _ ->
            input


addToInput : NumberSystem -> Value -> NumberSystem
addToInput (NumberSystem name outputs inputs) value =
    inputs
        |> mapSelected (addValueToNumberSystemInput (selected outputs) value)
        |> NumberSystem name outputs


parseRoman : List RomanDigit -> GroupedRomanDigits
parseRoman digits =
    parseRomanToList digits
        |> List.foldr addToRomanGroup emptyGroupedRomans


convert : NumberSystem -> OutputField
convert (NumberSystem _ outputs inputs) =
    makeConversion (selected inputs) (selected outputs)
        |> SingleStringOutputField


truncateDecimalToFourDigits : DecimalNumber -> FourDigitDecimalNumber
truncateDecimalToFourDigits (DecimalNumber digits) =
    case digits of
        a :: [] ->
            FourDigitDecimalNumber ( Zero, Zero, Zero, a )

        a :: b :: [] ->
            FourDigitDecimalNumber ( Zero, Zero, a, b )

        a :: b :: c :: [] ->
            FourDigitDecimalNumber ( Zero, a, b, c )

        a :: b :: c :: d :: [] ->
            if List.member a [ Zero, One, Two, Three ] then
                FourDigitDecimalNumber ( a, b, c, d )
            else
                FourDigitDecimalNumber ( Zero, Zero, Zero, Zero )

        _ ->
            FourDigitDecimalNumber ( Zero, Zero, Zero, Zero )


makeConversion : NumberSystemState -> NumberSystemOutput -> String
makeConversion input output =
    case ( input, output ) of
        ( RomanNumeralState number, RomanNumeral ) ->
            number
                |> romanNumeralToString

        ( RomanNumeralState number, Decimal ) ->
            number
                |> convertFromRoman
                |> decimalNumberToString

        ( RomanNumeralState number, Binary ) ->
            number
                |> convertFromRoman
                |> convertToBinary
                |> binaryNumberToString

        ( RomanNumeralState number, Hex ) ->
            number
                |> convertFromRoman
                |> convertToBinary
                |> convertBinaryToHex
                |> hexNumberToString

        ( DecimalState number, RomanNumeral ) ->
            number
                |> truncateDecimalToFourDigits
                |> convertToRoman
                |> romanNumeralToString

        ( DecimalState number, Decimal ) ->
            number
                |> decimalNumberToString

        ( DecimalState number, Binary ) ->
            number
                |> convertToBinary
                |> binaryNumberToString

        ( DecimalState number, Hex ) ->
            number
                |> convertToBinary
                |> convertBinaryToHex
                |> hexNumberToString

        ( BinaryState number, RomanNumeral ) ->
            number
                |> convertFromBinary
                |> truncateDecimalToFourDigits
                |> convertToRoman
                |> romanNumeralToString

        ( BinaryState number, Decimal ) ->
            number
                |> convertFromBinary
                |> decimalNumberToString

        ( BinaryState number, Binary ) ->
            number
                |> binaryNumberToString

        ( BinaryState number, Hex ) ->
            number
                |> convertBinaryToHex
                |> hexNumberToString

        ( HexState number, RomanNumeral ) ->
            number
                |> convertHexToBinary
                |> convertFromBinary
                |> truncateDecimalToFourDigits
                |> convertToRoman
                |> romanNumeralToString

        ( HexState number, Decimal ) ->
            number
                |> convertHexToBinary
                |> convertFromBinary
                |> decimalNumberToString

        ( HexState number, Binary ) ->
            number
                |> convertHexToBinary
                |> binaryNumberToString

        ( HexState number, Hex ) ->
            number
                |> hexNumberToString


mapInputNames : (String -> a) -> NumberSystem -> List a
mapInputNames f (NumberSystem _ _ inputs) =
    inputs
        |> toList
        |> List.map inputSystemName
        |> List.map f


mapOutputNames : (String -> a) -> NumberSystem -> List a
mapOutputNames f (NumberSystem _ outputs _) =
    outputs
        |> toList
        |> List.map outputSystemName
        |> List.map f


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


removeLeadingZeroes : digit -> List digit -> List digit
removeLeadingZeroes zero list =
    let
        helper e l =
            if (e == zero) then
                case l of
                    [] ->
                        []

                    _ ->
                        l ++ [ e ]
            else
                l ++ [ e ]

        keepOneZero l =
            case l of
                [] ->
                    [ zero ]

                _ ->
                    l
    in
        list
            |> List.foldl helper []
            |> keepOneZero


convertFromRoman : RomanNumber -> DecimalNumber
convertFromRoman (RomanNumber digits) =
    let
        grouped =
            parseRoman digits
    in
        convertRomanThousands grouped.thousands
            :: convertRomanDigitsToDigit ( C, D, M ) grouped.hundreds
            :: convertRomanDigitsToDigit ( X, L, C ) grouped.tens
            :: convertRomanDigitsToDigit ( I, V, X ) grouped.ones
            :: []
            |> removeLeadingZeroes Zero
            |> DecimalNumber


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


lessThan400 : List DecimalDigit -> Bool
lessThan400 digits =
    case digits of
        [] ->
            True

        a :: [] ->
            True

        a :: b :: [] ->
            True

        a :: b :: c :: [] ->
            List.member a [ Zero, One, Two, Three ]

        _ ->
            False


possibleNextDecimal : NumberSystemOutput -> DecimalNumber -> List DecimalDigit
possibleNextDecimal output (DecimalNumber numbers) =
    if lessThan400 numbers then
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
    else
        []


addDecimalDigit : NumberSystemOutput -> DecimalDigit -> DecimalNumber -> DecimalNumber
addDecimalDigit output digit ((DecimalNumber digits) as numbers) =
    if possibleNextDecimal output numbers |> List.member digit then
        if digits == [ Zero ] then
            DecimalNumber [ digit ]
        else
            digits
                ++ [ digit ]
                |> DecimalNumber
    else
        numbers


initDecimal : DecimalNumber
initDecimal =
    DecimalNumber [ Zero ]


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
        digits
            ++ [ digit ]
            |> RomanNumber
    else
        numbers


addBinaryDigit : BinaryDigit -> BinaryNumber -> BinaryNumber
addBinaryDigit digit (BinaryNumber digits) =
    case digits of
        BinaryZero :: [] ->
            BinaryNumber [ digit ]

        _ ->
            digits
                ++ [ digit ]
                |> BinaryNumber


addHexDigit : HexDigit -> HexNumber -> HexNumber
addHexDigit digit (HexNumber digits) =
    case digits of
        HexZero :: [] ->
            HexNumber [ digit ]

        _ ->
            digits
                ++ [ digit ]
                |> HexNumber


backspaceBinary : BinaryNumber -> BinaryNumber
backspaceBinary (BinaryNumber numbers) =
    case numbers of
        a :: [] ->
            BinaryNumber [ BinaryZero ]

        _ ->
            numbers
                |> List.take ((List.length numbers) - 1)
                |> BinaryNumber


backspaceDecimal : DecimalNumber -> DecimalNumber
backspaceDecimal (DecimalNumber digits) =
    if List.length digits <= 1 then
        DecimalNumber [ Zero ]
    else
        digits
            |> List.take (List.length digits - 1)
            |> DecimalNumber


backspaceRoman : RomanNumber -> RomanNumber
backspaceRoman (RomanNumber numbers) =
    numbers
        |> List.take ((List.length numbers) - 1)
        |> RomanNumber


backspaceHex : HexNumber -> HexNumber
backspaceHex (HexNumber digits) =
    if List.length digits <= 1 then
        HexNumber [ HexZero ]
    else
        digits
            |> List.take (List.length digits - 1)
            |> HexNumber


backspace : NumberSystem -> NumberSystem
backspace (NumberSystem name outputs inputs) =
    let
        helper : NumberSystemState -> NumberSystemState
        helper state =
            case state of
                RomanNumeralState number ->
                    number
                        |> backspaceRoman
                        |> RomanNumeralState

                DecimalState number ->
                    number
                        |> backspaceDecimal
                        |> DecimalState

                BinaryState number ->
                    number
                        |> backspaceBinary
                        |> BinaryState

                HexState number ->
                    number
                        |> backspaceHex
                        |> HexState
    in
        inputs
            |> mapSelected helper
            |> NumberSystem name outputs


initRoman : RomanNumber
initRoman =
    RomanNumber []


initBinary : BinaryNumber
initBinary =
    BinaryNumber [ BinaryZero ]


initHex : HexNumber
initHex =
    HexNumber [ HexZero ]


intToDecimal : Int -> DecimalNumber
intToDecimal n =
    let
        helper : Char -> DecimalDigit
        helper x =
            case x of
                '1' ->
                    One

                '2' ->
                    Two

                '3' ->
                    Three

                '4' ->
                    Four

                '5' ->
                    Five

                '6' ->
                    Six

                '7' ->
                    Seven

                '8' ->
                    Eight

                '9' ->
                    Nine

                _ ->
                    Zero
    in
        n
            |> Basics.toString
            |> String.foldr (\c list -> helper c :: list) []
            |> DecimalNumber


decimalToInt : DecimalNumber -> Int
decimalToInt (DecimalNumber decimalDigits) =
    let
        helper : List DecimalDigit -> Int
        helper digits =
            case digits of
                [] ->
                    0

                a :: rest ->
                    let
                        calcHelper i =
                            (i * (10 ^ (List.length rest))) + helper rest
                    in
                        case a of
                            Zero ->
                                helper rest

                            One ->
                                calcHelper 1

                            Two ->
                                calcHelper 2

                            Three ->
                                calcHelper 3

                            Four ->
                                calcHelper 4

                            Five ->
                                calcHelper 5

                            Six ->
                                calcHelper 6

                            Seven ->
                                calcHelper 7

                            Eight ->
                                calcHelper 8

                            Nine ->
                                calcHelper 9
    in
        helper decimalDigits


convertFromBinary : BinaryNumber -> DecimalNumber
convertFromBinary (BinaryNumber binaryDigits) =
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
        binaryDigits
            |> helper
            |> intToDecimal


convertToBinary : DecimalNumber -> BinaryNumber
convertToBinary number =
    let
        nextTwoFactor : Int -> Int -> Int
        nextTwoFactor total exponent =
            if 2 ^ exponent > total then
                exponent - 1
            else
                exponent
                    + 1
                    |> nextTwoFactor total

        helperRec : Int -> Int -> List BinaryDigit
        helperRec exponent res =
            if exponent < 0 then
                []
            else if res >= 2 ^ exponent then
                BinaryOne :: helperRec (exponent - 1) (res - 2 ^ exponent)
            else
                BinaryZero :: helperRec (exponent - 1) res

        int =
            decimalToInt number
    in
        int
            |> helperRec (nextTwoFactor int 1)
            |> BinaryNumber


convertHexDigitToBinary : HexDigit -> List BinaryDigit
convertHexDigitToBinary digit =
    case digit of
        HexOne ->
            [ BinaryZero, BinaryZero, BinaryZero, BinaryOne ]

        HexTwo ->
            [ BinaryZero, BinaryZero, BinaryOne, BinaryZero ]

        HexThree ->
            [ BinaryZero, BinaryZero, BinaryOne, BinaryOne ]

        HexFour ->
            [ BinaryZero, BinaryOne, BinaryZero, BinaryZero ]

        HexFive ->
            [ BinaryZero, BinaryOne, BinaryZero, BinaryOne ]

        HexSix ->
            [ BinaryZero, BinaryOne, BinaryOne, BinaryZero ]

        HexSeven ->
            [ BinaryZero, BinaryOne, BinaryOne, BinaryOne ]

        HexEight ->
            [ BinaryOne, BinaryZero, BinaryZero, BinaryZero ]

        HexNine ->
            [ BinaryOne, BinaryZero, BinaryZero, BinaryOne ]

        HexTen ->
            [ BinaryOne, BinaryZero, BinaryOne, BinaryZero ]

        HexEleven ->
            [ BinaryOne, BinaryZero, BinaryOne, BinaryOne ]

        HexTwelve ->
            [ BinaryOne, BinaryOne, BinaryZero, BinaryZero ]

        HexThirteen ->
            [ BinaryOne, BinaryOne, BinaryZero, BinaryOne ]

        HexFourteen ->
            [ BinaryOne, BinaryOne, BinaryOne, BinaryZero ]

        HexFifteen ->
            [ BinaryOne, BinaryOne, BinaryOne, BinaryOne ]

        HexZero ->
            [ BinaryZero, BinaryZero, BinaryZero, BinaryZero ]


convertHexToBinary : HexNumber -> BinaryNumber
convertHexToBinary (HexNumber number) =
    number
        |> List.concatMap convertHexDigitToBinary
        |> removeLeadingZeroes BinaryZero
        |> BinaryNumber


convertBinaryDigitsToHexDigit : BinaryDigit -> BinaryDigit -> BinaryDigit -> BinaryDigit -> HexDigit
convertBinaryDigitsToHexDigit a b c d =
    case ( a, b, c, d ) of
        ( BinaryZero, BinaryZero, BinaryZero, BinaryOne ) ->
            HexOne

        ( BinaryZero, BinaryZero, BinaryOne, BinaryZero ) ->
            HexTwo

        ( BinaryZero, BinaryZero, BinaryOne, BinaryOne ) ->
            HexThree

        ( BinaryZero, BinaryOne, BinaryZero, BinaryZero ) ->
            HexFour

        ( BinaryZero, BinaryOne, BinaryZero, BinaryOne ) ->
            HexFive

        ( BinaryZero, BinaryOne, BinaryOne, BinaryZero ) ->
            HexSix

        ( BinaryZero, BinaryOne, BinaryOne, BinaryOne ) ->
            HexSeven

        ( BinaryOne, BinaryZero, BinaryZero, BinaryZero ) ->
            HexEight

        ( BinaryOne, BinaryZero, BinaryZero, BinaryOne ) ->
            HexNine

        ( BinaryOne, BinaryZero, BinaryOne, BinaryZero ) ->
            HexTen

        ( BinaryOne, BinaryZero, BinaryOne, BinaryOne ) ->
            HexEleven

        ( BinaryOne, BinaryOne, BinaryZero, BinaryZero ) ->
            HexTwelve

        ( BinaryOne, BinaryOne, BinaryZero, BinaryOne ) ->
            HexThirteen

        ( BinaryOne, BinaryOne, BinaryOne, BinaryZero ) ->
            HexFourteen

        ( BinaryOne, BinaryOne, BinaryOne, BinaryOne ) ->
            HexFifteen

        ( BinaryZero, BinaryZero, BinaryZero, BinaryZero ) ->
            HexZero


convertRestOfBinaryToHex : { temp : List BinaryDigit, acc : List HexDigit } -> List HexDigit
convertRestOfBinaryToHex data =
    case data.temp of
        a :: [] ->
            (convertBinaryDigitsToHexDigit BinaryZero BinaryZero BinaryZero a) :: data.acc

        a :: b :: [] ->
            (convertBinaryDigitsToHexDigit BinaryZero BinaryZero a b) :: data.acc

        a :: b :: c :: [] ->
            (convertBinaryDigitsToHexDigit BinaryZero a b c) :: data.acc

        a :: b :: c :: d :: [] ->
            (convertBinaryDigitsToHexDigit a b c d) :: data.acc

        _ ->
            data.acc


convertBinaryToHex : BinaryNumber -> HexNumber
convertBinaryToHex (BinaryNumber number) =
    let
        t : BinaryDigit -> { temp : List BinaryDigit, acc : List HexDigit } -> { temp : List BinaryDigit, acc : List HexDigit }
        t digit data =
            case data.temp of
                a :: b :: c :: [] ->
                    { temp = [], acc = (convertBinaryDigitsToHexDigit digit a b c) :: data.acc }

                _ ->
                    { data
                        | temp = digit :: data.temp
                    }
    in
        number
            |> List.foldr t { temp = [], acc = [] }
            |> convertRestOfBinaryToHex
            |> HexNumber
