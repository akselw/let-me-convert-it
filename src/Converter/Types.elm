module Converter.Types exposing (..)

import SelectList exposing (SelectList)


type Converter
    = UnitConverter String (SelectList InputUnitState) (SelectList UnitType)
    | NumberSystemConverter String (SelectList NumberSystemState) (SelectList NumberSystem)



-- Unit Converters


type alias ComboInput =
    { major : String
    , minor : String
    , majorActive : Bool
    }


type InputUnitState
    = SingleInputState UnitDefinition String
    | ComboInputState UnitDefinition UnitDefinition ComboInput


type alias UnitDefinition =
    { factor : Float
    , name : String
    , abbreviation : String
    }


type UnitType
    = SingleUnit UnitDefinition
    | ComboUnit UnitDefinition UnitDefinition


type Field
    = SingleFloatField Float String
    | DoubleFloatField Float Float String String



-- Number Systems


type NumberSystem
    = RomanNumeral
    | Decimal


type NumberSystemState
    = RomanNumeralState RomanNumber
    | DecimalState FourDigitDecimalNumber


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


type RomanNumber
    = RomanNumber (List RomanDigit)


type BinaryNumber
    = BinaryNumber (List BinaryDigit)
