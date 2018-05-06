module Converters exposing (..)


type alias Converter =
    { name : String
    , units : List UnitType
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


converters : List Converter
converters =
    [ distance, weight, area ]


feetDefinition : Unit
feetDefinition =
    { factor = 0.3038
    , name = "feet"
    , abbreviation = "ft"
    }


inchDefinition : Unit
inchDefinition =
    { factor = 0.0254
    , name = "inch"
    , abbreviation = "\""
    }


meter : Unit
meter =
    { factor = 1
    , name = "meter"
    , abbreviation = "m"
    }


distance : Converter
distance =
    { name = "Distance"
    , units =
        [ SingleUnit meter
        , SingleUnit
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
    }


weight : Converter
weight =
    { name = "Weight"
    , units =
        [ SingleUnit
            { factor = 1
            , name = "kilogram"
            , abbreviation = "kg"
            }
        , SingleUnit
            { factor = 0.001
            , name = "gram"
            , abbreviation = "g"
            }
        , SingleUnit
            { factor = 1000
            , name = "metric ton"
            , abbreviation = ""
            }
        , SingleUnit
            { factor = 0.454
            , name = "pound"
            , abbreviation = "lb"
            }
        ]
    }


area : Converter
area =
    { name = "Area"
    , units =
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
    }
