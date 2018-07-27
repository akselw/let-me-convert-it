module Converter.Fields exposing (..)

-- Input


type InputField
    = SingleUnitInputField SingleUnitInputDict
    | DoubleUnitInputField DoubleUnitInputDict
    | SingleStringInputField String


type alias SingleUnitInputDict =
    { value : String
    , unit : String
    }


type alias DoubleUnitInputDict =
    { major : SingleUnitInputDict
    , minor : SingleUnitInputDict
    }



-- Output


type OutputField
    = SingleFloatOutputField SingleUnitOutputDict
    | DoubleFloatOutputField DoubleUnitOutputDict
    | SingleStringOutputField String


type alias SingleUnitOutputDict =
    { value : Float
    , unit : String
    }


type alias DoubleUnitOutputDict =
    { major : SingleUnitOutputDict
    , minor : SingleUnitOutputDict
    }
