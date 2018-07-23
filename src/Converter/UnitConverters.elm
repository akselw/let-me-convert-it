module Converter.UnitConverters
    exposing
        ( inputName
        , outputName
        , convert
        , defaultConverter
        , restOfConverters
        , addComma
        , addNumber
        , backspace
        , setMajorState
        )

import SelectList exposing (..)
import Converter.Types exposing (..)


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


backspace : InputUnitState -> InputUnitState
backspace inputState =
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


setMajorState : Bool -> InputUnitState -> InputUnitState
setMajorState active inputState =
    case inputState of
        SingleInputState _ _ ->
            inputState

        ComboInputState major minor input ->
            ComboInputState major minor { input | majorActive = active }


defaultConverter : Converter
defaultConverter =
    distance


restOfConverters : List Converter
restOfConverters =
    [ weight, area ]


inputName : InputUnitState -> String
inputName inputState =
    case inputState of
        SingleInputState def _ ->
            def.name

        ComboInputState majorDef minorDef _ ->
            majorDef.name ++ " - " ++ minorDef.name


outputName : UnitType -> String
outputName unit =
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


convert : InputUnitState -> UnitType -> Field
convert input output =
    let
        ( major, minor ) =
            makeConversion input output
    in
        case output of
            SingleUnit def ->
                SingleFloatField major def.abbreviation

            ComboUnit majorDef minorDef ->
                DoubleFloatField major minor majorDef.abbreviation minorDef.abbreviation


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


meter : UnitDefinition
meter =
    { factor = 1
    , name = "meter"
    , abbreviation = "m"
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


distance : Converter
distance =
    UnitConverter "Distance"
        (SelectList.fromLists
            []
            (SingleInputState meter "1")
            (List.map toInputState restOfDistanceUnits)
        )
        (SelectList.fromLists
            []
            (SingleUnit meter)
            restOfDistanceUnits
        )


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


poundDefinition =
    { factor = 0.454
    , name = "pound"
    , abbreviation = "lb"
    }


gramDefinition =
    { factor = 0.001
    , name = "gram"
    , abbreviation = "g"
    }


weight : Converter
weight =
    UnitConverter "Weight"
        (SelectList.fromLists
            []
            (SingleInputState poundDefinition "0")
            (List.map toInputState restOfWeightUnits)
        )
        (SelectList.fromLists
            []
            (SingleUnit gramDefinition)
            restOfWeightUnits
        )


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
    , SingleUnit
        poundDefinition
    ]


squareFootDefinition =
    { factor = 0.09290304
    , name = "square foot"
    , abbreviation = ""
    }


squareMeterDefinition =
    { factor = 1
    , name = "square meter"
    , abbreviation = "m²"
    }


area : Converter
area =
    UnitConverter "Area"
        (SelectList.fromLists
            []
            (SingleInputState squareFootDefinition "0")
            (List.map toInputState restOfAreaUnits)
        )
        (SelectList.fromLists
            []
            (SingleUnit squareMeterDefinition)
            restOfAreaUnits
        )


restOfAreaUnits : List UnitType
restOfAreaUnits =
    [ SingleUnit
        { factor = 1
        , name = "square meter"
        , abbreviation = "m²"
        }
    , SingleUnit
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
        , abbreviation = ""
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
        , abbreviation = ""
        }
    , SingleUnit
        { factor = 0.09290304
        , name = "square foot"
        , abbreviation = ""
        }
    , SingleUnit
        { factor = 2589988.110336
        , name = "square mile"
        , abbreviation = ""
        }
    ]
