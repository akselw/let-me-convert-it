module NumberSystems
    exposing
        ( RomanDigit(..)
        , DecimalDigit(..)
        , RomanNumber
        , DecimalNumber
        , convertToRoman
        , convertFromRoman
        , initRoman
        , initDecimal
        , addRomanDigit
        , addDecimalDigit
        , possibleNextRoman
        , possibleNextDecimal
        , parseTest
        )


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


type DecimalNumber
    = DecimalNumber ( DecimalDigit, DecimalDigit, DecimalDigit, DecimalDigit )


type RomanNumber
    = RomanNumber (List RomanDigit)


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


convertFromRoman : RomanNumber -> DecimalNumber
convertFromRoman (RomanNumber digits) =
    let
        grouped =
            parseRoman digits
    in
        DecimalNumber
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


convertToRoman : DecimalNumber -> RomanNumber
convertToRoman (DecimalNumber ( thousands, hundreds, tens, ones )) =
    (convertThousands thousands)
        ++ (convertDigitToRomanDigit ( C, D, M ) hundreds)
        ++ (convertDigitToRomanDigit ( X, L, C ) tens)
        ++ (convertDigitToRomanDigit ( I, V, X ) ones)
        |> RomanNumber


possibleNextDecimal : DecimalNumber -> List DecimalDigit
possibleNextDecimal (DecimalNumber ( thousands, hundreds, tens, ones )) =
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


addDecimalDigit : DecimalDigit -> DecimalNumber -> DecimalNumber
addDecimalDigit digit ((DecimalNumber ( thousands, hundreds, tens, ones )) as numbers) =
    if possibleNextDecimal numbers |> List.member digit then
        DecimalNumber ( hundreds, tens, ones, digit )
    else
        numbers


initDecimal : DecimalNumber
initDecimal =
    DecimalNumber ( Zero, Zero, Zero, Zero )


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


possibleSingle : RomanDigit -> List RomanDigit
possibleSingle digit =
    case digit of
        I ->
            [ I, V, X ]

        V ->
            [ I, V ]

        X ->
            [ I, V, X, L, C ]

        L ->
            [ I, V, X, L ]

        C ->
            allRomanDigits

        D ->
            [ I, V, X, L, C, D ]

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


possibleDouble : RomanDigit -> RomanDigit -> List RomanDigit
possibleDouble a b =
    if a == b then
        a :: smallerRomanDigits a
    else if smallerRomanDigits b |> List.member a then
        smallerRomanDigits a
    else if unaryRomanDigit b then
        possibleSingle b
    else
        smallerRomanDigits b


possibleTriple : RomanDigit -> RomanDigit -> RomanDigit -> List RomanDigit
possibleTriple a b c =
    if a == b && a == c then
        smallerRomanDigits c
    else if smallerRomanDigits a |> List.member b then
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


initRoman : RomanNumber
initRoman =
    RomanNumber []
