module Types exposing (..)

--
--type Converter
--    = UnitConverterType String UnitConverter
--    | FunctionConverterType String FunctionConverter
--
--
--type FunctionConverter
--    = NumberSystemConverter (List NumberSystem)
--
--
--type NumberSystem
--    = Roman
--    | Decimal
--
--
--type alias UnitConverter =
--    { units : List UnitType
--    , defaultInput : UnitType
--    , defaultOutput : UnitType
--    }
--
--
--type alias Factor =
--    Float
--


type alias Model =
    { selectionState : SelectionState
    , valgtConverter : ConverterState
    }


type SelectionState
    = Conversion
    | UnitSelection
    | InputSelection
    | OutputSelection



--
--type alias ComboInput =
--    { major : String
--    , minor : String
--    , majorActive : Bool
--    }
--
--
--type InputState
--    = SingleInputState Unit String
--    | ComboInputState Unit Unit ComboInput
--
--type alias ConverterState =
--    { converter : Converter
--    , input : InputState
--    , output : UnitType
--    }
