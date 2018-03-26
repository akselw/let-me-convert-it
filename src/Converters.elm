module Converters exposing (..)


type alias Converter =
    { name : String
    , siUnit : Unit
    , factors : List Unit
    }


type alias UnitName =
    { name : String
    , abbreviation : String
    }


type alias Factor =
    Float


type Unit
    = SiUnit UnitName
    | FactorUnit Float UnitName


converters : List Converter
converters =
    [ distance, weight ]


distance : Converter
distance =
    { name = "Distance"
    , siUnit =
        SiUnit
            { name = "meter"
            , abbreviation = "m"
            }
    , factors =
        [ FactorUnit 0.001
            { name = "millimeter"
            , abbreviation = "mm"
            }
        , FactorUnit 0.01
            { name = "centimeter"
            , abbreviation = "cm"
            }
        , FactorUnit 1000
            { name = "kilometer"
            , abbreviation = "km"
            }
        , FactorUnit 10000
            { name = "mil"
            , abbreviation = "mil"
            }
        , FactorUnit 0.0254
            { name = "inch"
            , abbreviation = "\""
            }
        , FactorUnit 0.3038
            { name = "feet"
            , abbreviation = "ft"
            }
        , FactorUnit 0.9144
            { name = "yard"
            , abbreviation = "mi"
            }
        , FactorUnit 1609.34
            { name = "mile"
            , abbreviation = "mi"
            }
        , FactorUnit 1852
            { name = "nautical mile"
            , abbreviation = ""
            }
        ]
    }


weight : Converter
weight =
    { name = "Weight"
    , siUnit =
        SiUnit
            { name = "kilogram"
            , abbreviation = "kg"
            }
    , factors =
        [ FactorUnit 0.001
            { name = "gram"
            , abbreviation = "g"
            }
        , FactorUnit 1000
            { name = "metric ton"
            , abbreviation = ""
            }
        , FactorUnit 0.454
            { name = "pound"
            , abbreviation = "lb"
            }
        ]
    }
