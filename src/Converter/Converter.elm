module Converter.Converter
    exposing
        ( ConverterState
        , OutputField(..)
        , InputField(..)
        , Value
        , Values(..)
        , init
        , input
        , inputName
        , outputName
        , converterName
        , output
        , values
        , addToInput
        , toString
        , isActive
        , deleteFromInput
        , mapConverters
        , mapInputUnits
        , mapOutputUnits
        , selectConverter
        , selectInputUnit
        , selectOutputUnit
        )

import SelectList exposing (..)
import Converter.UnitConverters as UnitConverters exposing (..)
import Converter.Types as Internal exposing (..)
import Converter.NumberSystems as NumberSystems exposing (..)


type OutputField
    = SingleFloatOutputField Float String
    | DoubleFloatOutputField Float Float String String
    | SingleStringOutputField String


type InputField
    = SingleUnitInputField String String
    | DoubleUnitInputField String String String String
    | SingleStringInputField String


type ConverterState
    = ConverterState (SelectList Converter)


type CommaOrMinor
    = Comma Bool
    | Minor UnitDefinition


type Value
    = IntValue String Bool
    | CommaOrMinorValue CommaOrMinor
    | DecimalValue DecimalDigit Bool
    | RomanValue RomanDigit Bool


type alias FloatDict =
    { zero : Value
    , one : Value
    , two : Value
    , three : Value
    , four : Value
    , five : Value
    , six : Value
    , seven : Value
    , eight : Value
    , nine : Value
    , transform : Value
    }


type alias IntDict =
    { zero : Value
    , one : Value
    , two : Value
    , three : Value
    , four : Value
    , five : Value
    , six : Value
    , seven : Value
    , eight : Value
    , nine : Value
    }


type alias RomanDict =
    { m : Value
    , d : Value
    , c : Value
    , l : Value
    , x : Value
    , v : Value
    , i : Value
    }


type Values
    = FloatValues FloatDict
    | IntValues IntDict
    | RomanValues RomanDict


init : ConverterState
init =
    UnitConverters.restOfConverters
        ++ [ NumberSystems.converter ]
        |> SelectList.fromLists [] UnitConverters.defaultConverter
        |> ConverterState


input : ConverterState -> InputField
input (ConverterState converters) =
    case selected converters of
        UnitConverter _ inputs _ ->
            case selected inputs of
                SingleInputState unit value ->
                    SingleUnitInputField value unit.abbreviation

                ComboInputState majorDef minorDef field ->
                    if field.majorActive then
                        SingleUnitInputField field.major majorDef.abbreviation
                    else
                        DoubleUnitInputField field.major field.minor majorDef.abbreviation minorDef.abbreviation

        NumberSystemConverter _ inputs _ ->
            inputs
                |> selected
                |> NumberSystems.input
                |> SingleStringInputField


convertFromInternalField : Internal.Field -> OutputField
convertFromInternalField field =
    case field of
        Internal.SingleFloatField f n ->
            SingleFloatOutputField f n

        Internal.DoubleFloatField majorF minorF majorS minorS ->
            DoubleFloatOutputField majorF minorF majorS minorS


getSelectedInputName : Converter -> String
getSelectedInputName converter =
    case converter of
        UnitConverter _ inputs _ ->
            inputs
                |> selected
                |> UnitConverters.inputName

        NumberSystemConverter _ inputs _ ->
            inputs
                |> selected
                |> NumberSystems.inputName


inputName : ConverterState -> String
inputName (ConverterState converters) =
    converters
        |> selected
        |> getSelectedInputName


getSelectedOutputName : Converter -> String
getSelectedOutputName converter =
    case converter of
        UnitConverter _ _ outputs ->
            outputs
                |> selected
                |> UnitConverters.outputName

        NumberSystemConverter _ _ outputs ->
            outputs
                |> selected
                |> NumberSystems.outputName


outputName : ConverterState -> String
outputName (ConverterState converters) =
    converters
        |> selected
        |> getSelectedOutputName


converterName : ConverterState -> String
converterName (ConverterState converters) =
    converters
        |> selected
        |> getConverterName


output : ConverterState -> OutputField
output (ConverterState converters) =
    case selected converters of
        UnitConverter _ inputs outputs ->
            UnitConverters.convert (selected inputs) (selected outputs)
                |> convertFromInternalField

        NumberSystemConverter _ inputs outputs ->
            NumberSystems.convert (selected inputs) (selected outputs)
                |> SingleStringOutputField


unitValues : InputUnitState -> FloatDict
unitValues inputState =
    let
        default : FloatDict
        default =
            { zero = IntValue "0" True
            , one = IntValue "1" True
            , two = IntValue "2" True
            , three = IntValue "3" True
            , four = IntValue "4" True
            , five = IntValue "5" True
            , six = IntValue "6" True
            , seven = IntValue "7" True
            , eight = IntValue "8" True
            , nine = IntValue "9" True
            , transform = Comma True |> CommaOrMinorValue
            }
    in
        case inputState of
            SingleInputState _ _ ->
                default

            ComboInputState _ minorDef input ->
                if input.majorActive then
                    { default | transform = Minor minorDef |> CommaOrMinorValue }
                else if String.contains "," input.minor then
                    { default | transform = Comma False |> CommaOrMinorValue }
                else
                    default


romanValue : RomanNumber -> RomanDigit -> Value
romanValue number digit =
    number
        |> possibleNextRoman
        |> List.member digit
        |> RomanValue digit


decimalValue : FourDigitDecimalNumber -> DecimalDigit -> Value
decimalValue number digit =
    number
        |> possibleNextDecimal
        |> List.member digit
        |> DecimalValue digit


values : ConverterState -> Values
values (ConverterState converters) =
    case selected converters of
        UnitConverter _ inputs _ ->
            inputs
                |> selected
                |> unitValues
                |> FloatValues

        NumberSystemConverter _ inputs _ ->
            case (selected inputs) of
                DecimalState number ->
                    IntValues
                        { zero = decimalValue number Zero
                        , one = decimalValue number One
                        , two = decimalValue number Two
                        , three = decimalValue number Three
                        , four = decimalValue number Four
                        , five = decimalValue number Five
                        , six = decimalValue number Six
                        , seven = decimalValue number Seven
                        , eight = decimalValue number Eight
                        , nine = decimalValue number Nine
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


addValueToUnitInput : Value -> InputUnitState -> InputUnitState
addValueToUnitInput value input =
    let
        l =
            Debug.log (toString value)
    in
        case value of
            IntValue number active ->
                UnitConverters.addNumber number input

            CommaOrMinorValue commaOrMinor ->
                case commaOrMinor of
                    Comma _ ->
                        UnitConverters.addComma input

                    Minor _ ->
                        UnitConverters.setMajorState False input

            DecimalValue _ _ ->
                input

            RomanValue _ _ ->
                input


addValueToNumberSystemInput : Value -> NumberSystemState -> NumberSystemState
addValueToNumberSystemInput value input =
    let
        l =
            Debug.log (toString value)
    in
        case ( input, value ) of
            ( DecimalState number, DecimalValue digit _ ) ->
                number
                    |> addDecimalDigit digit
                    |> DecimalState

            ( RomanNumeralState number, RomanValue digit _ ) ->
                number
                    |> addRomanDigit digit
                    |> RomanNumeralState

            _ ->
                input


addToInput : ConverterState -> Value -> ConverterState
addToInput (ConverterState converters) v =
    let
        t converter =
            case converter of
                UnitConverter name inputs outputs ->
                    UnitConverter name (mapSelected (addValueToUnitInput v) inputs) outputs

                NumberSystemConverter name inputs outputs ->
                    NumberSystemConverter name (mapSelected (addValueToNumberSystemInput v) inputs) outputs
    in
        mapSelected t converters
            |> ConverterState


toString : Value -> String
toString value =
    case value of
        IntValue s _ ->
            s

        CommaOrMinorValue e ->
            case e of
                Comma active ->
                    ","

                Minor minorDef ->
                    "+ " ++ minorDef.name

        DecimalValue digit _ ->
            NumberSystems.digitToString digit

        RomanValue digit _ ->
            NumberSystems.romanDigitToString digit


isActive : Value -> Bool
isActive value =
    case value of
        IntValue _ active ->
            active

        CommaOrMinorValue e ->
            case e of
                Comma active ->
                    active

                Minor minorDef ->
                    True

        DecimalValue _ active ->
            active

        RomanValue _ active ->
            active


deleteFromInput : ConverterState -> ConverterState
deleteFromInput (ConverterState converters) =
    let
        t converter =
            case converter of
                UnitConverter name inputs outputs ->
                    UnitConverter name (mapSelected UnitConverters.backspace inputs) outputs

                NumberSystemConverter name inputs outputs ->
                    NumberSystemConverter name (mapSelected NumberSystems.backspace inputs) outputs
    in
        mapSelected t converters
            |> ConverterState


getConverterName : Converter -> String
getConverterName converter =
    case converter of
        UnitConverter name _ _ ->
            name

        NumberSystemConverter name _ _ ->
            name


mapConverters : (String -> a) -> ConverterState -> List a
mapConverters f (ConverterState converters) =
    toList converters
        |> List.map getConverterName
        |> List.map f


mapSelected : (a -> a) -> SelectList a -> SelectList a
mapSelected f s =
    s
        |> mapBy
            (\position elem ->
                case position of
                    Selected ->
                        f elem

                    _ ->
                        elem
            )


mapInputUnits : (String -> a) -> ConverterState -> List a
mapInputUnits f (ConverterState converters) =
    let
        converter =
            selected converters
    in
        case converter of
            UnitConverter _ inputs _ ->
                toList inputs
                    |> List.map UnitConverters.inputName
                    |> List.map f

            NumberSystemConverter _ inputs _ ->
                toList inputs
                    |> List.map NumberSystems.inputName
                    |> List.map f


mapOutputUnits : (String -> a) -> ConverterState -> List a
mapOutputUnits f (ConverterState converters) =
    let
        converter =
            selected converters
    in
        case converter of
            UnitConverter _ _ outputs ->
                toList outputs
                    |> List.map UnitConverters.outputName
                    |> List.map f

            NumberSystemConverter _ _ outputs ->
                toList outputs
                    |> List.map NumberSystems.outputName
                    |> List.map f


selectConverter : ConverterState -> String -> ConverterState
selectConverter (ConverterState converters) name =
    converters
        |> select (\converter -> ((getConverterName converter) == name))
        |> ConverterState


selectInputUnit : ConverterState -> String -> ConverterState
selectInputUnit (ConverterState converters) name =
    let
        f converter =
            case converter of
                UnitConverter converterName inputs outputs ->
                    UnitConverter converterName (selectNewInputUnit name inputs) outputs

                NumberSystemConverter converterName inputs outputs ->
                    NumberSystemConverter converterName (NumberSystems.selectInput name inputs) outputs
    in
        converters
            |> mapSelected f
            |> ConverterState


selectOutputUnit : ConverterState -> String -> ConverterState
selectOutputUnit (ConverterState converters) name =
    let
        f converter =
            case converter of
                UnitConverter converterName inputs outputs ->
                    outputs
                        |> selectNewOutputUnit name
                        |> UnitConverter converterName inputs

                NumberSystemConverter converterName inputs outputs ->
                    outputs
                        |> NumberSystems.selectOutput name
                        |> NumberSystemConverter converterName inputs
    in
        converters
            |> mapSelected f
            |> ConverterState
