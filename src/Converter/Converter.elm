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


type OutputField
    = SingleFloatOutputField Float String
    | DoubleFloatOutputField Float Float String String


type InputField
    = SingleStringOutputField String String
    | DoubleStringOutputField String String String String


type ConverterState
    = ConverterState (SelectList Converter)


type CommaOrMinor
    = Comma Bool
    | Minor UnitDefinition


type Value
    = IntValue String Bool
    | CommaOrMinorValue CommaOrMinor


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


type Values
    = FloatValues FloatDict


init : ConverterState
init =
    ConverterState (SelectList.fromLists [] UnitConverters.defaultConverter UnitConverters.restOfConverters)


input : ConverterState -> InputField
input (ConverterState converters) =
    case selected converters of
        UnitConverter _ inputs _ ->
            case selected inputs of
                SingleInputState unit value ->
                    SingleStringOutputField value unit.abbreviation

                ComboInputState majorDef minorDef field ->
                    if field.majorActive then
                        SingleStringOutputField field.major majorDef.abbreviation
                    else
                        DoubleStringOutputField field.major field.minor majorDef.abbreviation minorDef.abbreviation


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


values : ConverterState -> Values
values (ConverterState converters) =
    case selected converters of
        UnitConverter _ inputs _ ->
            inputs
                |> selected
                |> unitValues
                |> FloatValues


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


addToInput : ConverterState -> Value -> ConverterState
addToInput (ConverterState converters) v =
    let
        t converter =
            case converter of
                UnitConverter name inputs outputs ->
                    UnitConverter name (mapSelected (addValueToUnitInput v) inputs) outputs
    in
        mapSelected t converters
            |> ConverterState


toString : Value -> String
toString value =
    case value of
        IntValue s active ->
            s

        CommaOrMinorValue e ->
            case e of
                Comma active ->
                    ","

                Minor minorDef ->
                    "+ " ++ minorDef.name


deleteFromInput : ConverterState -> ConverterState
deleteFromInput (ConverterState converters) =
    let
        t converter =
            case converter of
                UnitConverter name inputs outputs ->
                    UnitConverter name (mapSelected UnitConverters.backspace inputs) outputs
    in
        mapSelected t converters
            |> ConverterState


getConverterName : Converter -> String
getConverterName converter =
    case converter of
        UnitConverter name _ _ ->
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


selectConverter : ConverterState -> String -> ConverterState
selectConverter (ConverterState converters) name =
    converters
        |> select (\converter -> ((getConverterName converter) == name))
        |> ConverterState


selectNewInputUnit : SelectList InputUnitState -> String -> SelectList InputUnitState
selectNewInputUnit inputs name =
    select (\input -> (UnitConverters.inputName input) == name) inputs


selectInputUnit : ConverterState -> String -> ConverterState
selectInputUnit (ConverterState converters) name =
    let
        f converter =
            case converter of
                UnitConverter converterName inputs outputs ->
                    UnitConverter converterName (selectNewInputUnit inputs name) outputs
    in
        converters
            |> mapSelected f
            |> ConverterState


selectNewOutputUnit : String -> SelectList UnitType -> SelectList UnitType
selectNewOutputUnit name outputs =
    select (\output -> (UnitConverters.outputName output) == name) outputs


selectOutputUnit : ConverterState -> String -> ConverterState
selectOutputUnit (ConverterState converters) name =
    let
        f converter =
            case converter of
                UnitConverter converterName inputs outputs ->
                    outputs
                        |> selectNewOutputUnit name
                        |> UnitConverter converterName inputs
    in
        converters
            |> mapSelected f
            |> ConverterState
