module Converter.Types exposing (..)

import SelectList exposing (SelectList)


type Converter
    = UnitConverter String (SelectList InputUnitState) (SelectList UnitType)


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
