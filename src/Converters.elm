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
    [ distance, weight, area ]


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


area : Converter
area =
    { name = "Area"
    , siUnit =
        SiUnit
            { name = "square meter"
            , abbreviation = "m²"
            }
    , factors =
        [ FactorUnit 0.000001
            { name = "square millimeter"
            , abbreviation = "mm²"
            }
        , FactorUnit 0.0001
            { name = "square centimeter"
            , abbreviation = "cm²"
            }
        , FactorUnit 1000
            { name = "mål"
            , abbreviation = ""
            }
        , FactorUnit 10000
            { name = "hectare"
            , abbreviation = ""
            }
        , FactorUnit 1000000
            { name = "square kilometer"
            , abbreviation = "km²"
            }
        , FactorUnit 0.0
            { name = "square centimeter"
            , abbreviation = "cm²"
            }
        , FactorUnit 0.00064516
            { name = "square inch"
            , abbreviation = ""
            }
        , FactorUnit 0.09290304
            { name = "square foot"
            , abbreviation = ""
            }
        , FactorUnit 2589988.110336
            { name = "square mile"
            , abbreviation = ""
            }
        ]
    }
