module Converters exposing (..)


type alias Converter =
    { name : String
    , siUnit : SiDefinition
    , factors : List Unit
    }


type alias UnitName =
    { name : String
    , abbreviation : String
    }


type alias Factor =
    Float


type alias SiDefinition =
    UnitName


type alias FactorDefinition =
    { factor : Float
    , name : UnitName
    }


type alias ComboDefinition =
    { majorFactor : Float
    , minorFactor : Float
    , comboName : String
    , majorName : UnitName
    , minorName : UnitName
    }


type Unit
    = SiUnit SiDefinition
    | FactorUnit FactorDefinition
    | ComboUnit ComboDefinition


converters : List Converter
converters =
    [ distance, weight, area ]


feet : Unit
feet =
    FactorUnit
        { factor = 0.3038
        , name =
            { name = "feet"
            , abbreviation = "ft"
            }
        }


inches : Unit
inches =
    FactorUnit
        { factor = 0.0254
        , name =
            { name = "inch"
            , abbreviation = "\""
            }
        }


distance : Converter
distance =
    { name = "Distance"
    , siUnit =
        { name = "meter"
        , abbreviation = "m"
        }
    , factors =
        [ FactorUnit
            { factor = 0.001
            , name =
                { name = "millimeter"
                , abbreviation = "mm"
                }
            }
        , FactorUnit
            { factor = 0.01
            , name =
                { name = "centimeter"
                , abbreviation = "cm"
                }
            }
        , FactorUnit
            { factor = 1000
            , name =
                { name = "kilometer"
                , abbreviation = "km"
                }
            }
        , FactorUnit
            { factor = 10000
            , name =
                { name = "mil"
                , abbreviation = "mil"
                }
            }
        , FactorUnit
            { factor = 0.0254
            , name =
                { name = "inch"
                , abbreviation = "\""
                }
            }
        , FactorUnit
            { factor = 0.3038
            , name =
                { name = "feet"
                , abbreviation = "ft"
                }
            }
        , FactorUnit
            { factor = 0.9144
            , name =
                { name = "yard"
                , abbreviation = "mi"
                }
            }
        , FactorUnit
            { factor = 1609.34
            , name =
                { name = "mile"
                , abbreviation = "mi"
                }
            }
        , FactorUnit
            { factor = 1852
            , name =
                { name = "nautical mile"
                , abbreviation = ""
                }
            }
        ]
    }


weight : Converter
weight =
    { name = "Weight"
    , siUnit =
        { name = "kilogram"
        , abbreviation = "kg"
        }
    , factors =
        [ FactorUnit
            { factor =
                0.001
            , name =
                { name = "gram"
                , abbreviation = "g"
                }
            }
        , FactorUnit
            { factor = 1000
            , name =
                { name = "metric ton"
                , abbreviation = ""
                }
            }
        , FactorUnit
            { factor = 0.454
            , name =
                { name = "pound"
                , abbreviation = "lb"
                }
            }
        ]
    }


area : Converter
area =
    { name = "Area"
    , siUnit =
        { name = "square meter"
        , abbreviation = "m²"
        }
    , factors =
        [ FactorUnit
            { factor = 0.000001
            , name =
                { name = "square millimeter"
                , abbreviation = "mm²"
                }
            }
        , FactorUnit
            { factor = 0.0001
            , name =
                { name = "square centimeter"
                , abbreviation = "cm²"
                }
            }
        , FactorUnit
            { factor = 1000
            , name =
                { name = "mål"
                , abbreviation = ""
                }
            }
        , FactorUnit
            { factor = 10000
            , name =
                { name = "hectare"
                , abbreviation = ""
                }
            }
        , FactorUnit
            { factor = 1000000
            , name =
                { name = "square kilometer"
                , abbreviation = "km²"
                }
            }
        , FactorUnit
            { factor = 0.0
            , name =
                { name = "square centimeter"
                , abbreviation = "cm²"
                }
            }
        , FactorUnit
            { factor = 0.00064516
            , name =
                { name = "square inch"
                , abbreviation = ""
                }
            }
        , FactorUnit
            { factor = 0.09290304
            , name =
                { name = "square foot"
                , abbreviation = ""
                }
            }
        , FactorUnit
            { factor = 2589988.110336
            , name =
                { name = "square mile"
                , abbreviation = ""
                }
            }
        ]
    }
