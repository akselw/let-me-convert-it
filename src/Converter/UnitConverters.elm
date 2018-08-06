module Converter.UnitConverters
    exposing
        ( UnitConverterState
          -- input/output
        , input
        , addToInput
        , backspace
        , convert
          --names
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
        , defaultConverter
        , restOfConverters
        )

import SelectList exposing (..)
import Common exposing (mapSelected)
import Converter.Fields exposing (..)
import Converter.Value exposing (..)
import Converter.Values exposing (..)


-- Types


type UnitConverterState
    = UnitConverterState String (SelectList UnitType) (SelectList InputUnitState)


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



-- Functions


addValueToUnitInput : Value -> InputUnitState -> InputUnitState
addValueToUnitInput value input =
    case value of
        IntValue number active ->
            addNumber number input

        CommaOrMinorValue commaOrMinor ->
            case commaOrMinor of
                Comma _ ->
                    addComma input

                Minor _ ->
                    setMajorState False input

        DecimalValue _ _ ->
            input

        RomanValue _ _ ->
            input

        BinaryValue _ _ ->
            input

        HexValue _ _ ->
            input


addToInput : UnitConverterState -> Value -> UnitConverterState
addToInput (UnitConverterState name outputs inputs) value =
    inputs
        |> mapSelected (addValueToUnitInput value)
        |> UnitConverterState name outputs


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
                    { default | transform = Minor minorDef.name |> CommaOrMinorValue }
                else if String.contains "," input.minor then
                    { default | transform = Comma False |> CommaOrMinorValue }
                else
                    default


values : UnitConverterState -> Values
values (UnitConverterState _ _ inputs) =
    inputs
        |> selected
        |> unitValues
        |> FloatValues


input : UnitConverterState -> InputField
input (UnitConverterState _ _ inputs) =
    case selected inputs of
        SingleInputState unit value ->
            SingleUnitInputField { value = value, unit = unit.abbreviation }

        ComboInputState majorDef minorDef field ->
            if field.majorActive then
                SingleUnitInputField { value = field.major, unit = majorDef.abbreviation }
            else
                DoubleUnitInputField
                    { major = { value = field.major, unit = majorDef.abbreviation }
                    , minor = { value = field.minor, unit = minorDef.abbreviation }
                    }


updateInputNumberPress : String -> String -> String
updateInputNumberPress oldInput numberPressed =
    if oldInput == "0" then
        numberPressed
    else
        oldInput ++ numberPressed


addNumber : String -> InputUnitState -> InputUnitState
addNumber numberString inputState =
    case inputState of
        SingleInputState unit input ->
            SingleInputState unit (updateInputNumberPress input numberString)

        ComboInputState major minor input ->
            if input.majorActive then
                ComboInputState major minor { input | major = (updateInputNumberPress input.major numberString) }
            else
                ComboInputState major minor { input | minor = (updateInputNumberPress input.minor numberString) }


updateInputCommaPress : String -> String
updateInputCommaPress input =
    if String.contains "." input then
        input
    else
        input ++ "."


addComma : InputUnitState -> InputUnitState
addComma inputState =
    case inputState of
        SingleInputState unit input ->
            SingleInputState unit (updateInputCommaPress input)

        ComboInputState major minor input ->
            ComboInputState major minor { input | minor = (updateInputCommaPress input.minor) }


updateInputBackspace : String -> String
updateInputBackspace input =
    if String.length input == 1 then
        "0"
    else
        String.dropRight 1 input


unitBackspace : InputUnitState -> InputUnitState
unitBackspace inputState =
    case inputState of
        SingleInputState unit input ->
            SingleInputState unit (updateInputBackspace input)

        ComboInputState major minor input ->
            if input.majorActive then
                ComboInputState major minor { input | major = (updateInputBackspace input.major) }
            else if input.minor == "0" then
                ComboInputState major
                    minor
                    { input
                        | minor = (updateInputBackspace input.minor)
                        , majorActive = True
                    }
            else
                ComboInputState major minor { input | minor = (updateInputBackspace input.minor) }


backspace : UnitConverterState -> UnitConverterState
backspace (UnitConverterState name outputs inputs) =
    inputs
        |> mapSelected unitBackspace
        |> UnitConverterState name outputs


setMajorState : Bool -> InputUnitState -> InputUnitState
setMajorState active inputState =
    case inputState of
        SingleInputState _ _ ->
            inputState

        ComboInputState major minor input ->
            ComboInputState major minor { input | majorActive = active }


inputName : UnitConverterState -> String
inputName (UnitConverterState _ _ inputs) =
    inputs
        |> selected
        |> inputUnitName


converterName : UnitConverterState -> String
converterName (UnitConverterState name _ _) =
    name


mapInputNames : (String -> a) -> UnitConverterState -> List a
mapInputNames f (UnitConverterState _ _ inputs) =
    inputs
        |> toList
        |> List.map inputUnitName
        |> List.map f


mapOutputNames : (String -> a) -> UnitConverterState -> List a
mapOutputNames f (UnitConverterState _ outputs _) =
    outputs
        |> toList
        |> List.map outputUnitName
        |> List.map f


inputUnitName : InputUnitState -> String
inputUnitName inputState =
    case inputState of
        SingleInputState def _ ->
            def.name

        ComboInputState majorDef minorDef _ ->
            majorDef.name ++ " and " ++ minorDef.name


outputName : UnitConverterState -> String
outputName (UnitConverterState _ outputs _) =
    outputs
        |> selected
        |> outputUnitName


outputUnitName : UnitType -> String
outputUnitName unit =
    case unit of
        SingleUnit def ->
            def.name

        ComboUnit major minor ->
            major.name ++ " and " ++ minor.name


convertInputString : Float -> String -> Float
convertInputString factor input =
    let
        inputRes =
            String.toFloat input
    in
        case inputRes of
            Ok i ->
                i * factor

            Err s ->
                -1000


convertFromInput : InputUnitState -> Float
convertFromInput inputState =
    case inputState of
        SingleInputState unit input ->
            convertInputString unit.factor input

        ComboInputState majorUnit minorUnit { major, minor } ->
            (convertInputString majorUnit.factor major) + (convertInputString minorUnit.factor minor)


convertToOutput : UnitType -> Float -> ( Float, Float )
convertToOutput unitType n =
    case unitType of
        SingleUnit unit ->
            ( n / unit.factor, 0 )

        ComboUnit majorUnit minorUnit ->
            let
                majorRaw =
                    n / majorUnit.factor

                major =
                    majorRaw |> floor |> toFloat

                majorRest =
                    majorRaw - major

                minor =
                    majorRest * majorUnit.factor / minorUnit.factor
            in
                ( major, minor )


makeConversion : InputUnitState -> UnitType -> ( Float, Float )
makeConversion input output =
    input
        |> convertFromInput
        |> convertToOutput output


convert : UnitConverterState -> OutputField
convert (UnitConverterState _ outputs inputs) =
    let
        input =
            selected inputs

        output =
            selected outputs

        ( major, minor ) =
            makeConversion input output
    in
        case output of
            SingleUnit def ->
                SingleFloatOutputField { value = major, unit = def.abbreviation }

            ComboUnit majorDef minorDef ->
                DoubleFloatOutputField
                    { major = { value = major, unit = majorDef.abbreviation }
                    , minor = { value = minor, unit = minorDef.abbreviation }
                    }


feetDefinition : UnitDefinition
feetDefinition =
    { factor = 0.3038
    , name = "feet"
    , abbreviation = "ft"
    }


inchDefinition : UnitDefinition
inchDefinition =
    { factor = 0.0254
    , name = "inch"
    , abbreviation = "\""
    }


toInputState : UnitType -> InputUnitState
toInputState unit =
    case unit of
        SingleUnit definition ->
            SingleInputState definition "0"

        ComboUnit major minor ->
            ComboInputState major
                minor
                { major = "0"
                , minor = "0"
                , majorActive = True
                }


selectNewInputUnit : String -> SelectList InputUnitState -> SelectList InputUnitState
selectNewInputUnit name inputs =
    select (\input -> (inputUnitName input) == name) inputs


selectInput : String -> UnitConverterState -> UnitConverterState
selectInput name (UnitConverterState converterName outputs inputs) =
    inputs
        |> selectNewInputUnit name
        |> UnitConverterState converterName outputs


inputUnits : InputUnitState -> List UnitType -> String -> SelectList InputUnitState
inputUnits first rest selection =
    rest
        |> List.map toInputState
        |> SelectList.fromLists [] first
        |> selectNewInputUnit selection


selectNewOutputUnit : String -> SelectList UnitType -> SelectList UnitType
selectNewOutputUnit name outputs =
    select (\output -> (outputUnitName output) == name) outputs


selectOutput : String -> UnitConverterState -> UnitConverterState
selectOutput name (UnitConverterState converterName outputs inputs) =
    UnitConverterState converterName (selectNewOutputUnit name outputs) inputs


outputUnits : UnitType -> List UnitType -> String -> SelectList UnitType
outputUnits first rest selection =
    rest
        |> SelectList.fromLists [] first
        |> selectNewOutputUnit selection



-- Converters


defaultConverter : UnitConverterState
defaultConverter =
    distance


restOfConverters : List UnitConverterState
restOfConverters =
    [ weight, area ]


distance : UnitConverterState
distance =
    UnitConverterState "Distance"
        (outputUnits (SingleUnit meter) restOfDistanceUnits "centimeter")
        (inputUnits (SingleInputState meter "1") restOfDistanceUnits "feet and inch")


meter : UnitDefinition
meter =
    { factor = 1
    , name = "meter"
    , abbreviation = "m"
    }


restOfDistanceUnits : List UnitType
restOfDistanceUnits =
    [ SingleUnit
        { factor = 0.01
        , name = "centimeter"
        , abbreviation = "cm"
        }
    , SingleUnit
        { factor = 1000
        , name = "kilometer"
        , abbreviation = "km"
        }
    , SingleUnit
        { factor = 10000
        , name = "mil"
        , abbreviation = "mil"
        }
    , SingleUnit inchDefinition
    , SingleUnit feetDefinition
    , ComboUnit feetDefinition inchDefinition
    , SingleUnit
        { factor = 0.9144
        , name = "yard"
        , abbreviation = "mi"
        }
    , SingleUnit
        { factor = 1609.34
        , name = "mile"
        , abbreviation = "mi"
        }
    , SingleUnit
        { factor = 1852
        , name = "nautical mile"
        , abbreviation = ""
        }
    ]


poundDefinition : UnitDefinition
poundDefinition =
    { factor = 0.454
    , name = "pound"
    , abbreviation = "lb"
    }


gramDefinition : UnitDefinition
gramDefinition =
    { factor = 0.001
    , name = "gram"
    , abbreviation = "g"
    }


weight : UnitConverterState
weight =
    UnitConverterState "Weight"
        (outputUnits (SingleUnit poundDefinition) restOfWeightUnits "gram")
        (inputUnits (SingleInputState poundDefinition "1") restOfWeightUnits "pound")


restOfWeightUnits : List UnitType
restOfWeightUnits =
    [ SingleUnit
        { factor = 1
        , name = "kilogram"
        , abbreviation = "kg"
        }
    , SingleUnit
        gramDefinition
    , SingleUnit
        { factor = 1000
        , name = "metric ton"
        , abbreviation = ""
        }
    ]


squareMeterDefinition : UnitDefinition
squareMeterDefinition =
    { factor = 1
    , name = "square meter"
    , abbreviation = "m²"
    }


area : UnitConverterState
area =
    UnitConverterState "Area"
        (outputUnits (SingleUnit squareMeterDefinition) restOfAreaUnits "square foot")
        (inputUnits (SingleInputState squareMeterDefinition "1") restOfAreaUnits "square meter")


restOfAreaUnits : List UnitType
restOfAreaUnits =
    [ SingleUnit
        { factor = 0.000001
        , name = "square millimeter"
        , abbreviation = "mm²"
        }
    , SingleUnit
        { factor = 0.0001
        , name = "square centimeter"
        , abbreviation = "cm²"
        }
    , SingleUnit
        { factor = 1000
        , name = "mål"
        , abbreviation = "mål"
        }
    , SingleUnit
        { factor = 10000
        , name = "hectare"
        , abbreviation = ""
        }
    , SingleUnit
        { factor = 1000000
        , name = "square kilometer"
        , abbreviation = "km²"
        }
    , SingleUnit
        { factor = 0.0
        , name = "square centimeter"
        , abbreviation = "cm²"
        }
    , SingleUnit
        { factor = 0.00064516
        , name = "square inch"
        , abbreviation = "sq in"
        }
    , SingleUnit
        { factor = 0.09290304
        , name = "square foot"
        , abbreviation = "sq ft"
        }
    , SingleUnit
        { factor = 2589988.110336
        , name = "square mile"
        , abbreviation = "sq mi"
        }
    ]
