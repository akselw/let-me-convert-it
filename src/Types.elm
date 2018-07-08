module Types exposing (..)


type alias Converter =
    { name : String
    , units : List UnitType
    , defaultInput : UnitType
    , defaultOutput : UnitType
    }


type alias Factor =
    Float


type alias Unit =
    { factor : Factor
    , name : String
    , abbreviation : String
    }


type UnitType
    = SingleUnit Unit
    | ComboUnit Unit Unit


type alias Model =
    { converters : List Converter
    , selectionState : SelectionState
    , valgtConverter : ConverterState
    }


type SelectionState
    = Conversion
    | UnitSelection
    | InputSelection
    | OutputSelection


type alias ComboInput =
    { major : String
    , minor : String
    , majorActive : Bool
    }


type InputState
    = SingleInputState Unit String
    | ComboInputState Unit Unit ComboInput


type alias ConverterState =
    { converter : Converter
    , input : InputState
    , output : UnitType
    }
