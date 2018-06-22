module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src, disabled, value)
import Html.Events exposing (..)
import List.Extra exposing (find)
import Converters exposing (..)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Style exposing (..)
import Style.Color as Color exposing (text)
import Style.Font as Font exposing (..)
import Color exposing (..)


---- MODEL ----


(=>) =
    (,)


type alias Test =
    {}


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


erTall : (Int -> Int) -> (String -> Result String String)
erTall func =
    \inp ->
        case String.toInt inp of
            Ok n ->
                Ok (toString (func n))

            Err e ->
                Err e


init : ( Model, Cmd Msg )
init =
    ( { converters = converters
      , valgtConverter =
            { converter = distance
            , input = SingleInputState Converters.meter "1"
            , output = ComboUnit Converters.feetDefinition Converters.inchDefinition
            }
      , selectionState = Conversion
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = MeassurableChanged Converter
    | InputUnitChanged UnitType
    | OutputUnitChanged UnitType
    | NumberPressed String
    | CommaPressed
    | BackspacePressed
    | MinorPressed
    | SelectionStateChanged SelectionState


sameUnitName : String -> (UnitType -> Bool)
sameUnitName valgtNavn =
    \unitType ->
        case unitType of
            SingleUnit unit ->
                valgtNavn == unit.name

            ComboUnit major minor ->
                valgtNavn == (major.name ++ " and " ++ minor.name)


findUnit : String -> ConverterState -> Maybe UnitType
findUnit valgtNavn converterState =
    find (sameUnitName valgtNavn) converterState.converter.units


createComboTuple : String -> ( Maybe String, Maybe String ) -> ( Maybe String, Maybe String )
createComboTuple inputString tuple =
    let
        minorInput =
            Tuple.second tuple
    in
        if inputString == "" then
            ( Nothing, minorInput )
        else
            ( Just inputString, minorInput )



--updatedMinorInput : String -> ComboDefinition -> ( Maybe String, Maybe String ) -> InputState
--updatedMinorInput inputString definition tuple =
--    let
--        inputFelt =
--            if inputString == "" then
--                Nothing
--            else
--                Just inputString
--
--        newTuple =
--            ( Tuple.first tuple, inputFelt )
--    in
--        ComboState definition newTuple


createNewInputState : UnitType -> ComboInput -> InputState
createNewInputState newUnit state =
    case newUnit of
        SingleUnit unit ->
            SingleInputState unit state.major

        ComboUnit majorUnit minorUnit ->
            ComboInputState majorUnit minorUnit state


newInputState : UnitType -> InputState -> InputState
newInputState newUnit oldState =
    case oldState of
        SingleInputState unit input ->
            createNewInputState newUnit { major = input, minor = "0", majorActive = True }

        ComboInputState majorUnit minorUnit state ->
            createNewInputState newUnit state


updateInputNumberPress : String -> String -> String
updateInputNumberPress oldInput numberPressed =
    if oldInput == "0" then
        numberPressed
    else
        oldInput ++ numberPressed


numberPress : String -> InputState -> InputState
numberPress numberString inputState =
    case inputState of
        SingleInputState unit input ->
            SingleInputState unit (updateInputNumberPress input numberString)

        ComboInputState major minor input ->
            if input.majorActive then
                ComboInputState major minor { input | major = (updateInputNumberPress input.major numberString) }
            else
                ComboInputState major minor { input | minor = (updateInputNumberPress input.minor numberString) }


updateInputCommaPress : String -> String
updateInputCommaPress input =
    if String.contains "." input then
        input
    else
        input ++ "."


commaPress : InputState -> InputState
commaPress state =
    case state of
        SingleInputState unit input ->
            SingleInputState unit (updateInputCommaPress input)

        ComboInputState major minor input ->
            ComboInputState major minor { input | minor = (updateInputCommaPress input.minor) }


updateInputBackspacePress : String -> String
updateInputBackspacePress input =
    if String.length input == 1 then
        "0"
    else
        String.dropRight 1 input


backspacePress : InputState -> InputState
backspacePress state =
    case state of
        SingleInputState unit input ->
            SingleInputState unit (updateInputBackspacePress input)

        ComboInputState major minor input ->
            if input.majorActive then
                ComboInputState major minor { input | major = (updateInputBackspacePress input.major) }
            else if input.minor == "0" then
                ComboInputState major
                    minor
                    { input
                        | minor = (updateInputBackspacePress input.minor)
                        , majorActive = True
                    }
            else
                ComboInputState major minor { input | minor = (updateInputBackspacePress input.minor) }


update : Msg -> Model -> Model
update msg model =
    let
        converterState =
            model.valgtConverter
    in
        case msg of
            MeassurableChanged converter ->
                { model
                    | valgtConverter =
                        { converter = converter
                        , input = newInputState converter.defaultInput converterState.input
                        , output = converter.defaultOutput
                        }
                    , selectionState = Conversion
                }

            InputUnitChanged input ->
                { model
                    | valgtConverter =
                        { converterState
                            | input = (newInputState input model.valgtConverter.input)
                        }
                    , selectionState = Conversion
                }

            OutputUnitChanged output ->
                { model
                    | valgtConverter = { converterState | output = output }
                    , selectionState = Conversion
                }

            SelectionStateChanged state ->
                { model | selectionState = state }

            NumberPressed numberString ->
                { model | valgtConverter = { converterState | input = (numberPress numberString converterState.input) } }

            CommaPressed ->
                { model | valgtConverter = { converterState | input = (commaPress converterState.input) } }

            BackspacePressed ->
                { model | valgtConverter = { converterState | input = (backspacePress converterState.input) } }

            MinorPressed ->
                case converterState.input of
                    SingleInputState _ _ ->
                        model

                    ComboInputState major minor inputState ->
                        { model | valgtConverter = { converterState | input = ComboInputState major minor { inputState | majorActive = False } } }



---- VIEW ----


lagConverterOption : Converter -> Html Msg
lagConverterOption converter =
    option [] [ Html.text converter.name ]


lagUnitOption : UnitType -> Html Msg
lagUnitOption unitType =
    case unitType of
        SingleUnit unit ->
            option [] [ Html.text unit.name ]

        ComboUnit major minor ->
            option [] [ Html.text (major.name ++ " and " ++ minor.name) ]


inputSelect converterValg =
    div []
        [--- select [ Html.Events.onInput InputUnitChanged ] (List.map lagUnitOption converterValg.converter.units)
        ]


outputSelect converterValg =
    div []
        [--- select [ Html.Events.onInput OutputUnitChanged ] (List.map lagUnitOption converterValg.converter.units)
        ]


convertInputString : Factor -> String -> Float
convertInputString factor input =
    let
        inputRes =
            String.toFloat input
    in
        case inputRes of
            Ok i ->
                i * factor

            Err s ->
                -1000


convertFromInput : InputState -> Float
convertFromInput inputState =
    case inputState of
        SingleInputState unit input ->
            convertInputString unit.factor input

        ComboInputState majorUnit minorUnit { major, minor } ->
            (convertInputString majorUnit.factor major) + (convertInputString minorUnit.factor minor)


convertToOutput : UnitType -> Float -> ( Float, Float )
convertToOutput unitType n =
    case unitType of
        SingleUnit unit ->
            ( n / unit.factor, 0 )

        ComboUnit majorUnit minorUnit ->
            let
                majorRaw =
                    n / majorUnit.factor

                major =
                    majorRaw |> floor |> toFloat

                majorRest =
                    majorRaw - major

                minor =
                    majorRest * majorUnit.factor / minorUnit.factor
            in
                ( major, minor )


makeConversion : ConverterState -> ( Float, Float )
makeConversion converterState =
    converterState.input
        |> convertFromInput
        |> convertToOutput converterState.output


removeFormatting : String -> String
removeFormatting input =
    input |> String.split " " |> String.join ""


type MyStyles
    = None
    | InputStyle
    | ButtonStyle
    | Background



-- We define our stylesheet


stylesheet =
    Style.styleSheet
        [ Style.style None []
        , Style.style InputStyle
            [ Color.text black
            , Color.background lightGray
            , Font.size 20 -- all units given as px
            , Font.alignRight
            ]
        , Style.style ButtonStyle
            [ Color.text white
            , Color.background darkGray
            , Font.size 20 -- all units given as px
            ]
        , Style.style Background
            [ Color.background blue
            ]
        ]


numberButtons : Int -> List (OnGrid (Element MyStyles variation Msg))
numberButtons offset =
    List.map
        (\num ->
            let
                numberString =
                    toString num
            in
                cell
                    { start = ( (num - 1) % 3, (2 - ((num - 1) // 3)) + offset )
                    , width = 1
                    , height = 1
                    , content = el ButtonStyle [ Element.Events.onClick (NumberPressed numberString), width fill, Element.Attributes.center ] (Element.text numberString)
                    }
        )
        [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ]


inputText : InputState -> String
inputText inputState =
    case inputState of
        SingleInputState unit input ->
            input ++ " " ++ unit.abbreviation

        ComboInputState majorUnit minorUnit { major, minor, majorActive } ->
            if majorActive then
                major ++ " " ++ majorUnit.abbreviation
            else
                major ++ " " ++ majorUnit.abbreviation ++ " + " ++ minor ++ " " ++ minorUnit.abbreviation


inputUnitName : InputState -> String
inputUnitName inputState =
    case inputState of
        SingleInputState unit _ ->
            unit.name

        ComboInputState major minor _ ->
            major.name ++ " and " ++ minor.name


outputUnitName : UnitType -> String
outputUnitName output =
    case output of
        SingleUnit unit ->
            unit.name

        ComboUnit major minor ->
            major.name ++ " and " ++ minor.name


commaMinorButtonElement : InputState -> Element MyStyles variation Msg
commaMinorButtonElement inputState =
    case inputState of
        SingleInputState _ _ ->
            el ButtonStyle [ Element.Events.onClick CommaPressed ] (Element.text ",")

        ComboInputState _ minor input ->
            if input.majorActive then
                el ButtonStyle [ Element.Events.onClick MinorPressed ] (Element.text ("+ " ++ minor.name))
            else
                el ButtonStyle [ Element.Events.onClick CommaPressed ] (Element.text ",")


addUnit : Unit -> Float -> String
addUnit unit n =
    (toString n) ++ " " ++ unit.abbreviation


addUnitToTuple : Unit -> Unit -> ( Float, Float ) -> String
addUnitToTuple majorUnit minorUnit ( major, minor ) =
    (addUnit majorUnit major) ++ " " ++ (addUnit minorUnit minor)


outputElement : ConverterState -> Element MyStyles variation Msg
outputElement converterState =
    let
        conversionResult =
            makeConversion converterState
    in
        case converterState.output of
            SingleUnit unit ->
                conversionResult
                    |> Tuple.first
                    |> addUnit unit
                    |> Element.text
                    |> el InputStyle [ width (percent 100), paddingRight 8 ]

            ComboUnit majorUnit minorUnit ->
                if (Tuple.second conversionResult == 0) then
                    conversionResult
                        |> Tuple.first
                        |> addUnit majorUnit
                        |> Element.text
                        |> el InputStyle [ width (percent 100), paddingRight 8 ]
                else
                    conversionResult
                        |> addUnitToTuple majorUnit minorUnit
                        |> Element.text
                        |> el InputStyle [ width (percent 100), paddingRight 8 ]


calc : Model -> Element MyStyles variation Msg
calc model =
    let
        outputBox =
            outputElement model.valgtConverter
    in
        grid None
            [ padding 7, spacing 20, width (percent 100), Element.Attributes.center ]
            { columns = [ fill, fill, fill, fill ]
            , rows =
                [ px 50
                , px 75
                , px 75
                , px 75
                , px 75
                , px 75
                , px 75
                ]
            , cells =
                (numberButtons 3)
                    ++ [ cell
                            { start = ( 0, 0 )
                            , width = 2
                            , height = 1
                            , content = el InputStyle [ Element.Events.onClick CommaPressed, width (percent 100), Element.Attributes.center ] (Element.text (inputUnitName model.valgtConverter.input))
                            }
                       , cell
                            { start = ( 2, 0 )
                            , width = 2
                            , height = 1
                            , content = el InputStyle [ Element.Events.onClick CommaPressed, width (percent 100), Element.Attributes.center ] (Element.text (outputUnitName model.valgtConverter.output))
                            }
                       , cell
                            { start = ( 0, 1 )
                            , width = 4
                            , height = 1
                            , content = el InputStyle [ Element.Events.onClick CommaPressed, width (percent 100), Element.Attributes.center ] (Element.text (inputText model.valgtConverter.input))
                            }
                       , cell
                            { start = ( 0, 2 )
                            , width = 4
                            , height = 1
                            , content = el InputStyle [ Element.Events.onClick CommaPressed, width (percent 100), Element.Attributes.center ] outputBox
                            }
                       , cell
                            { start = ( 0, 6 )
                            , width = 1
                            , height = 1
                            , content = commaMinorButtonElement model.valgtConverter.input
                            }
                       , cell
                            { start = ( 1, 6 )
                            , width = 1
                            , height = 1
                            , content = el ButtonStyle [ Element.Events.onClick (NumberPressed "0") ] (Element.text "0")
                            }
                       , cell
                            { start = ( 2, 6 )
                            , width = 1
                            , height = 1
                            , content = el ButtonStyle [ Element.Events.onClick BackspacePressed ] (Element.text "<-")
                            }
                       ]
            }


viewUnits : Converter -> Element MyStyles variation Msg
viewUnits converter =
    Element.row Background
        [ minHeight (px 75), paddingLeft 8, verticalCenter, Element.Events.onClick (MeassurableChanged converter) ]
        [ Element.text converter.name
        ]


getName : UnitType -> String
getName unitType =
    case unitType of
        SingleUnit unit ->
            unit.name

        ComboUnit major minor ->
            major.name ++ " " ++ minor.name


viewInputUnit : (UnitType -> Msg) -> UnitType -> Element MyStyles variation Msg
viewInputUnit msg unit =
    Element.row Background
        [ minHeight (px 75), paddingLeft 8, verticalCenter, Element.Events.onClick (msg unit) ]
        [ Element.text (getName unit)
        ]


unitRow : String -> String -> String -> List (Element MyStyles variation Msg)
unitRow converterName inputName outputName =
    [ row None [ minHeight (px 75), verticalCenter, paddingLeft 8, Element.Events.onClick (SelectionStateChanged UnitSelection) ] [ Element.text converterName ]
    , row Background
        [ minHeight (px 75), verticalCenter ]
        [ column Background [ minWidth (percent 50), paddingLeft 8, Element.Events.onClick (SelectionStateChanged InputSelection) ] [ Element.text inputName ]
        , column Background [ minWidth (percent 50), paddingLeft 8, Element.Events.onClick (SelectionStateChanged OutputSelection) ] [ Element.text outputName ]
        ]
    ]


inputOutputRow : ConverterState -> List (Element MyStyles variation Msg)
inputOutputRow converterState =
    [ row InputStyle [ minWidth (percent 100), minHeight (px 75), verticalCenter ] [ Element.el InputStyle [ width (percent 100), paddingRight 8 ] (Element.text (inputText converterState.input)) ]
    , row InputStyle [ minWidth (percent 100), minHeight (px 75), verticalCenter ] [ outputElement converterState ]
    ]


calc2 : Model -> Element MyStyles variation Msg
calc2 model =
    case model.selectionState of
        UnitSelection ->
            column None
                []
                (List.map viewUnits model.converters)

        InputSelection ->
            column None
                []
                (((unitRow
                    model.valgtConverter.converter.name
                    "-"
                    (outputUnitName model.valgtConverter.output)
                  )
                    ++ (inputOutputRow model.valgtConverter)
                 )
                    ++ (List.map (viewInputUnit InputUnitChanged) model.valgtConverter.converter.units)
                )

        OutputSelection ->
            column None
                []
                (((unitRow
                    model.valgtConverter.converter.name
                    (inputUnitName model.valgtConverter.input)
                    "-"
                  )
                    ++ (inputOutputRow model.valgtConverter)
                 )
                    ++ (List.map (viewInputUnit OutputUnitChanged) model.valgtConverter.converter.units)
                )

        Conversion ->
            column None
                []
                ((unitRow
                    model.valgtConverter.converter.name
                    (inputUnitName model.valgtConverter.input)
                    (outputUnitName model.valgtConverter.output)
                 )
                    ++ (inputOutputRow model.valgtConverter)
                )


calculator : Model -> Html Msg
calculator model =
    Element.layout stylesheet <|
        -- An el is the most basic element, like a <div>
        calc2 model



-- Element.layout renders the elements as html.
-- Every layout requires a stylesheet.
--
--unitSelector : Model -> Html Msg
--unitSelector model =
--    div []
--        (select [ Html.Events.onInput MeassurableChanged ]
--            (option [] [ Html.text "---------" ]
--                :: List.map lagConverterOption model.converters
--            )
--            :: converterView model
--        )
--


view : Model -> Html Msg
view model =
    div []
        [ calculator model
        ]


converterView : Model -> List (Html Msg)
converterView model =
    [ inputSelect model.valgtConverter
    , outputSelect model.valgtConverter
    ]


outputDisplay : ConverterState -> Model -> Html Msg
outputDisplay converterState model =
    let
        conversionResult =
            makeConversion converterState
    in
        div [] [ input [ disabled True, value (toString conversionResult) ] [] ]



---- PROGRAM ----


updateWithCmd : Msg -> Model -> ( Model, Cmd Msg )
updateWithCmd msg model =
    ( update msg model, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = updateWithCmd
        , subscriptions = always Sub.none
        }
