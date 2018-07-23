module Main exposing (..)

import Html exposing (Html, program)
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Style exposing (..)
import Style.Color as Color exposing (text)
import Style.Font as Font exposing (..)
import Style.Border as Border exposing (..)
import Color exposing (..)
import Round exposing (round)
import Converter.Converter exposing (..)


type alias Model =
    { selectionState : SelectionState
    , converterState : ConverterState
    }


type SelectionState
    = Conversion
    | ConverterSelection
    | InputSelection
    | OutputSelection


type ConverterMenuState
    = ConversionSelectionMenu String String String
    | InputSelectionMenu String String
    | OutputSelectionMenu String String



-- MODEL ----


init : ( Model, Cmd Msg )
init =
    ( { converterState = Converter.Converter.init
      , selectionState = Conversion
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ConverterChanged String
    | InputUnitChanged String
    | OutputUnitChanged String
    | ValueButtonPressed Value
    | BackspacePressed
    | SelectionStateChanged SelectionState


update : Msg -> Model -> Model
update msg model =
    case msg of
        ConverterChanged name ->
            { model
                | converterState = selectConverter model.converterState name
                , selectionState = Conversion
            }

        InputUnitChanged name ->
            { model
                | converterState = selectInputUnit model.converterState name
                , selectionState = Conversion
            }

        OutputUnitChanged name ->
            { model
                | converterState = selectOutputUnit model.converterState name
                , selectionState = Conversion
            }

        SelectionStateChanged state ->
            { model | selectionState = state }

        ValueButtonPressed value ->
            { model | converterState = addToInput model.converterState value }

        BackspacePressed ->
            { model | converterState = deleteFromInput model.converterState }



---- VIEW ----


type MyStyles
    = None
    | InputStyle
    | ButtonStyle
    | Background
    | UnitStyle


stylesheet : StyleSheet MyStyles variation
stylesheet =
    Style.styleSheet
        [ Style.style None []
        , Style.style InputStyle
            [ Color.text black
            , Color.background lightGray
            , Font.size 22 -- all units given as px
            , Font.alignRight
            , Font.typeface [ Font.sansSerif ]
            ]
        , Style.style ButtonStyle
            [ Color.text black
            , Color.background lightGray
            , Font.size 22 -- all units given as px
            , Font.center
            , Font.typeface [ Font.sansSerif ]
            , Border.solid
            , Border.all 1
            , Color.border darkGray
            , Border.rounded 16
            ]
        , Style.style Background
            [ Color.background white
            ]
        , Style.style UnitStyle
            [ Color.background lightGray
            , Font.typeface [ Font.sansSerif ]
            ]
        ]


decimalFormatLength : Float -> Int
decimalFormatLength number =
    let
        intLength =
            floor number |> Basics.toString |> String.length
    in
        if intLength <= 9 then
            9 - intLength
        else
            0


formatNumber : Float -> String
formatNumber number =
    let
        f =
            Round.round (decimalFormatLength number) number

        s =
            String.split "." f
    in
        case s of
            primary :: decimal :: rest ->
                let
                    p =
                        String.foldr
                            (\c s ->
                                if ((c == '0') && (String.length s == 0)) then
                                    ""
                                else
                                    (String.cons c s)
                            )
                            ""
                            decimal
                in
                    if (String.length p /= 0) then
                        primary ++ "." ++ p
                    else
                        primary

            _ ->
                f


viewConverter : String -> Element MyStyles variation Msg
viewConverter name =
    Element.row UnitStyle
        [ minHeight (px 75), paddingLeft 8, verticalCenter, Element.Events.onClick (ConverterChanged name) ]
        [ Element.text name
        ]


viewInputUnit : (String -> Msg) -> String -> Element MyStyles variation Msg
viewInputUnit msg unit =
    Element.row UnitStyle
        [ width fill, minHeight (px 75), paddingLeft 8, verticalCenter, Element.Events.onClick (msg unit) ]
        [ Element.text unit
        ]


unitRow : String -> String -> String -> Element MyStyles variation Msg
unitRow converterName inputName outputName =
    row None
        []
        [ column None
            [ width fill ]
            [ row UnitStyle [ minHeight (px 75), verticalCenter, paddingLeft 8, Element.Events.onClick (SelectionStateChanged ConverterSelection) ] [ Element.text converterName ]
            , row Background
                []
                [ column UnitStyle [ minHeight (px 75), verticalCenter, minWidth (percent 50), paddingLeft 8, Element.Events.onClick (SelectionStateChanged InputSelection) ] [ Element.text inputName ]
                , column UnitStyle [ minHeight (px 75), verticalCenter, minWidth (percent 50), paddingLeft 8, Element.Events.onClick (SelectionStateChanged OutputSelection) ] [ Element.text outputName ]
                ]
            ]
        ]


converterSelectionMenu : ConverterMenuState -> Element MyStyles variation Msg
converterSelectionMenu state =
    case state of
        ConversionSelectionMenu converter input output ->
            unitRow converter input output

        InputSelectionMenu converter output ->
            unitRow converter "-" output

        OutputSelectionMenu converter input ->
            unitRow converter input "-"


outputFieldToString : OutputField -> String
outputFieldToString field =
    case field of
        SingleFloatOutputField f s ->
            let
                number =
                    formatNumber f
            in
                number ++ " " ++ s

        DoubleFloatOutputField f1 f2 s1 s2 ->
            let
                major =
                    formatNumber f1

                minor =
                    formatNumber f2
            in
                String.join " " [ major, s1, minor, s2 ]


outputField : OutputField -> Element MyStyles variation Msg
outputField field =
    row InputStyle [ minWidth (percent 100), minHeight (px 75), verticalCenter ] [ Element.el InputStyle [ width (percent 100), paddingRight 8 ] (Element.text (outputFieldToString field)) ]


inputFieldToString : InputField -> String
inputFieldToString input =
    case input of
        SingleStringOutputField value unit ->
            String.join " " [ value, unit ]

        DoubleStringOutputField major minor majorUnit minorUnit ->
            String.join " " [ major, majorUnit, minor, minorUnit ]


inputField : InputField -> Element MyStyles variation Msg
inputField field =
    row InputStyle [ minWidth (percent 100), minHeight (px 75), verticalCenter ] [ Element.el InputStyle [ width (percent 100), paddingRight 8 ] (Element.text (inputFieldToString field)) ]


inputOutputRow : ConverterState -> Element MyStyles variation Msg
inputOutputRow converterState =
    row None
        []
        [ column None
            [ width fill ]
            [ inputField (input converterState)
            , outputField (output converterState)
            ]
        ]


buttonElement : Msg -> String -> Element MyStyles variation Msg
buttonElement msg s =
    column ButtonStyle [ minWidth (percent 31), verticalCenter, Element.Events.onClick msg, minHeight (px 100) ] [ Element.text s ]


numberButton : Value -> Element MyStyles variation Msg
numberButton value =
    value
        |> Converter.Converter.toString
        |> buttonElement (ValueButtonPressed value)


numberButtonRow : Value -> Value -> Value -> Element MyStyles variation Msg
numberButtonRow first second third =
    row Background
        [ width fill, minHeight (px 100), verticalCenter, spacing 8, paddingBottom 8 ]
        [ numberButton first
        , numberButton second
        , numberButton third
        ]


buttomButtonRow : Value -> Value -> Element MyStyles variation Msg
buttomButtonRow comma zero =
    row Background
        [ width fill, minHeight (px 100), verticalCenter, spacing 8, paddingBottom 8 ]
        [ numberButton comma
        , numberButton zero
        , buttonElement BackspacePressed "←"
        ]


valueButtons : ConverterState -> Element MyStyles variation Msg
valueButtons converterState =
    case values converterState of
        FloatValues valueDict ->
            row None
                []
                [ column None
                    [ width fill, padding 8 ]
                    [ numberButtonRow valueDict.seven valueDict.eight valueDict.nine
                    , numberButtonRow valueDict.four valueDict.five valueDict.six
                    , numberButtonRow valueDict.one valueDict.two valueDict.three
                    , buttomButtonRow valueDict.transform valueDict.zero
                    ]
                ]


viewConverterSelection : ConverterState -> Element MyStyles variation Msg
viewConverterSelection converterState =
    column None
        [ width fill ]
        (Converter.Converter.mapConverters viewConverter converterState)


viewInputs : ConverterState -> (String -> Element MyStyles variation Msg) -> Element MyStyles variation Msg
viewInputs converterState f =
    row None
        []
        [ mapInputUnits f converterState
            |> column None []
        ]


viewOutputs : ConverterState -> (String -> Element MyStyles variation Msg) -> Element MyStyles variation Msg
viewOutputs converterState f =
    row None
        []
        [ mapOutputUnits f converterState
            |> column Background [ width fill ]
        ]


viewInputSelection : ConverterState -> Element MyStyles variation Msg
viewInputSelection converterState =
    column None
        [ width fill ]
        [ InputSelectionMenu (converterName converterState) (outputName converterState)
            |> converterSelectionMenu
        , inputOutputRow converterState
        , viewInputUnit InputUnitChanged
            |> viewOutputs converterState
        ]


viewOutputSelection : ConverterState -> Element MyStyles variation Msg
viewOutputSelection converterState =
    column None
        [ width fill ]
        [ OutputSelectionMenu (converterName converterState) (inputName converterState)
            |> converterSelectionMenu
        , inputOutputRow converterState
        , viewInputUnit OutputUnitChanged
            |> viewOutputs converterState
        ]


viewConversion : ConverterState -> Element MyStyles variation Msg
viewConversion converterState =
    column None
        [ width fill ]
        [ ConversionSelectionMenu (converterName converterState)
            (inputName converterState)
            (outputName converterState)
            |> converterSelectionMenu
        , inputOutputRow converterState
        , valueButtons converterState
        ]


calculator : Model -> Element MyStyles variation Msg
calculator { selectionState, converterState } =
    case selectionState of
        ConverterSelection ->
            viewConverterSelection converterState

        InputSelection ->
            viewInputSelection converterState

        OutputSelection ->
            viewOutputSelection converterState

        Conversion ->
            viewConversion converterState


view : Model -> Html Msg
view model =
    Element.layout stylesheet <|
        calculator model



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
