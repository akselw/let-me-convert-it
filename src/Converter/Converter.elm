module Converter.Converter
    exposing
        ( ConverterState
        , Value
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
        , mapConverterNames
        , mapInputNames
        , mapOutputNames
        , selectConverter
        , selectInputUnit
        , selectOutputUnit
        )

import SelectList exposing (..)
import Common exposing (mapSelected)
import Converter.Value as Internal exposing (..)
import Converter.Fields exposing (..)
import Converter.Values exposing (..)
import Converter.UnitConverters as UnitConverters exposing (..)
import Converter.NumberSystems as NumberSystems exposing (..)


type ConverterState
    = ConverterState (SelectList Converter)


type Converter
    = UnitConverter UnitConverterState
    | NumberSystemConverter NumberSystem


type alias Value =
    Internal.Value


init : ConverterState
init =
    (UnitConverters.restOfConverters
        |> List.map UnitConverter
    )
        ++ [ NumberSystemConverter NumberSystems.converter ]
        |> SelectList.fromLists [] (UnitConverter UnitConverters.defaultConverter)
        |> ConverterState


input : ConverterState -> InputField
input (ConverterState converters) =
    case selected converters of
        UnitConverter state ->
            UnitConverters.input state

        NumberSystemConverter state ->
            NumberSystems.input state


getSelectedInputName : Converter -> String
getSelectedInputName converter =
    case converter of
        UnitConverter state ->
            UnitConverters.inputName state

        NumberSystemConverter state ->
            NumberSystems.inputName state


inputName : ConverterState -> String
inputName (ConverterState converters) =
    converters
        |> selected
        |> getSelectedInputName


getSelectedOutputName : Converter -> String
getSelectedOutputName converter =
    case converter of
        UnitConverter state ->
            UnitConverters.outputName state

        NumberSystemConverter state ->
            NumberSystems.outputName state


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
        UnitConverter state ->
            UnitConverters.convert state

        NumberSystemConverter state ->
            NumberSystems.convert state


values : ConverterState -> Values
values (ConverterState converters) =
    case selected converters of
        UnitConverter state ->
            UnitConverters.values state

        NumberSystemConverter state ->
            NumberSystems.values state


addToInput : ConverterState -> Value -> ConverterState
addToInput (ConverterState converters) value =
    let
        t converter =
            case converter of
                UnitConverter state ->
                    UnitConverters.addToInput state value
                        |> UnitConverter

                NumberSystemConverter state ->
                    NumberSystems.addToInput state value
                        |> NumberSystemConverter
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

                Minor minor ->
                    "+ " ++ minor

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

                Minor _ ->
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
                UnitConverter state ->
                    state
                        |> UnitConverters.backspace
                        |> UnitConverter

                NumberSystemConverter state ->
                    state
                        |> NumberSystems.backspace
                        |> NumberSystemConverter
    in
        mapSelected t converters
            |> ConverterState


getConverterName : Converter -> String
getConverterName converter =
    case converter of
        UnitConverter state ->
            UnitConverters.converterName state

        NumberSystemConverter state ->
            NumberSystems.converterName state


mapConverterNames : (String -> a) -> ConverterState -> List a
mapConverterNames f (ConverterState converters) =
    toList converters
        |> List.map getConverterName
        |> List.map f


mapInputNames : (String -> a) -> ConverterState -> List a
mapInputNames f (ConverterState converters) =
    case selected converters of
        UnitConverter state ->
            UnitConverters.mapInputNames f state

        NumberSystemConverter state ->
            NumberSystems.mapInputNames f state


mapOutputNames : (String -> a) -> ConverterState -> List a
mapOutputNames f (ConverterState converters) =
    case selected converters of
        UnitConverter state ->
            UnitConverters.mapOutputNames f state

        NumberSystemConverter state ->
            NumberSystems.mapOutputNames f state


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
                UnitConverter state ->
                    state
                        |> UnitConverters.selectInput name
                        |> UnitConverter

                NumberSystemConverter state ->
                    state
                        |> NumberSystems.selectInput name
                        |> NumberSystemConverter
    in
        converters
            |> mapSelected f
            |> ConverterState


selectOutputUnit : ConverterState -> String -> ConverterState
selectOutputUnit (ConverterState converters) name =
    let
        f converter =
            case converter of
                UnitConverter state ->
                    state
                        |> UnitConverters.selectOutput name
                        |> UnitConverter

                NumberSystemConverter state ->
                    state
                        |> NumberSystems.selectOutput name
                        |> NumberSystemConverter
    in
        converters
            |> mapSelected f
            |> ConverterState
