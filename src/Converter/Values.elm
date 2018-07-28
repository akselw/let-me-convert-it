module Converter.Values exposing (..)

import Converter.Value exposing (..)


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


type alias BinaryDict =
    { zero : Value
    , one : Value
    }


type alias HexDict =
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
    , ten : Value
    , eleven : Value
    , twelve : Value
    , thirteen : Value
    , fourteen : Value
    , fifteen : Value
    }


type Values
    = FloatValues FloatDict
    | IntValues IntDict
    | RomanValues RomanDict
    | BinaryValues BinaryDict
    | HexValues HexDict
