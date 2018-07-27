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


type Values
    = FloatValues FloatDict
    | IntValues IntDict
    | RomanValues RomanDict
