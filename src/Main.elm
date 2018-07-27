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
import Converter.Fields exposing (..)
import Converter.Values exposing (..)


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


type Variation
    = Disabled


stylesheet : StyleSheet MyStyles Variation
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
            , variation Disabled
                [ Color.background grey
                , Color.text darkGray
                ]
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


viewConverter : String -> Element MyStyles Variation Msg
viewConverter name =
    Element.row UnitStyle
        [ minHeight (px 75), paddingLeft 8, verticalCenter, Element.Events.onClick (ConverterChanged name) ]
        [ Element.text name
        ]


viewInputUnit : (String -> Msg) -> String -> Element MyStyles Variation Msg
viewInputUnit msg unit =
    Element.row UnitStyle
        [ width fill, minHeight (px 75), paddingLeft 8, verticalCenter, Element.Events.onClick (msg unit) ]
        [ Element.text unit
        ]


unitRow : String -> String -> String -> Element MyStyles Variation Msg
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


converterSelectionMenu : ConverterMenuState -> Element MyStyles Variation Msg
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
        SingleFloatOutputField { value, unit } ->
            let
                number =
                    formatNumber value
            in
                number ++ " " ++ unit

        DoubleFloatOutputField { major, minor } ->
            let
                majorValue =
                    formatNumber major.value

                minorValue =
                    formatNumber minor.value
            in
                String.join " " [ majorValue, major.unit, minorValue, minor.unit ]

        SingleStringOutputField s ->
            s


outputField : OutputField -> Element MyStyles Variation Msg
outputField field =
    row InputStyle [ minWidth (percent 100), minHeight (px 75), verticalCenter ] [ Element.el InputStyle [ width (percent 100), paddingRight 8 ] (Element.text (outputFieldToString field)) ]


inputFieldToString : InputField -> String
inputFieldToString input =
    case input of
        SingleUnitInputField { value, unit } ->
            String.join " " [ value, unit ]

        DoubleUnitInputField { major, minor } ->
            String.join " " [ major.value, major.unit, minor.value, minor.unit ]

        SingleStringInputField s ->
            s


inputField : InputField -> Element MyStyles Variation Msg
inputField field =
    row InputStyle [ minWidth (percent 100), minHeight (px 75), verticalCenter ] [ Element.el InputStyle [ width (percent 100), paddingRight 8 ] (Element.text (inputFieldToString field)) ]


inputOutputRow : ConverterState -> Element MyStyles Variation Msg
inputOutputRow converterState =
    row None
        []
        [ column None
            [ width fill ]
            [ inputField (input converterState)
            , outputField (output converterState)
            ]
        ]


buttonElement : Msg -> String -> Element MyStyles Variation Msg
buttonElement msg s =
    Element.button ButtonStyle
        [ minWidth (percent 31), verticalCenter, Element.Events.onClick msg, minHeight (px 100) ]
        (Element.text s)


wideButtonElement : Msg -> String -> Element MyStyles Variation Msg
wideButtonElement msg s =
    Element.button ButtonStyle
        [ minWidth (percent 64), verticalCenter, Element.Events.onClick msg, minHeight (px 100) ]
        (Element.text s)


disabledButtonElement : String -> Element MyStyles Variation Msg
disabledButtonElement s =
    Element.button ButtonStyle
        [ attribute "disabled" "true", minWidth (percent 31), verticalCenter, minHeight (px 100), vary Disabled True ]
        (Element.text s)


wideDisabledButtonElement : String -> Element MyStyles Variation Msg
wideDisabledButtonElement s =
    Element.button ButtonStyle
        [ attribute "disabled" "true", minWidth (percent 64), verticalCenter, minHeight (px 100), vary Disabled True ]
        (Element.text s)


romanButtonElement : Msg -> String -> Element MyStyles Variation Msg
romanButtonElement msg s =
    column ButtonStyle
        [ minWidth (percent 48), verticalCenter, Element.Events.onClick msg, minHeight (px 100) ]
        [ Element.text s ]


disabledRomanButtonElement : String -> Element MyStyles Variation Msg
disabledRomanButtonElement s =
    column ButtonStyle
        [ minWidth (percent 48), verticalCenter, minHeight (px 100), vary Disabled True ]
        [ Element.text s ]


numberButton : Value -> Element MyStyles Variation Msg
numberButton value =
    case isActive value of
        True ->
            value
                |> Converter.Converter.toString
                |> buttonElement (ValueButtonPressed value)

        False ->
            value
                |> Converter.Converter.toString
                |> disabledButtonElement


wideNumberButton : Value -> Element MyStyles Variation Msg
wideNumberButton value =
    case isActive value of
        True ->
            value
                |> Converter.Converter.toString
                |> wideButtonElement (ValueButtonPressed value)

        False ->
            value
                |> Converter.Converter.toString
                |> wideDisabledButtonElement


numberButtonRow : Value -> Value -> Value -> Element MyStyles Variation Msg
numberButtonRow first second third =
    row Background
        [ width fill, minHeight (px 100), verticalCenter, spacing 8, paddingBottom 8 ]
        [ numberButton first
        , numberButton second
        , numberButton third
        ]


romanButton : Value -> Element MyStyles Variation Msg
romanButton value =
    case isActive value of
        True ->
            value
                |> Converter.Converter.toString
                |> romanButtonElement (ValueButtonPressed value)

        False ->
            value
                |> Converter.Converter.toString
                |> disabledRomanButtonElement


romanButtonRow : Value -> Value -> Element MyStyles Variation Msg
romanButtonRow first second =
    row Background
        [ width fill, minHeight (px 100), verticalCenter, spacing 8, paddingBottom 8 ]
        [ romanButton first
        , romanButton second
        ]


buttomButtonRow : Value -> Value -> Element MyStyles Variation Msg
buttomButtonRow comma zero =
    row Background
        [ width fill, minHeight (px 100), verticalCenter, spacing 8, paddingBottom 8 ]
        [ numberButton comma
        , numberButton zero
        , buttonElement BackspacePressed "←"
        ]


wideButtomButtonRow : Value -> Element MyStyles Variation Msg
wideButtomButtonRow zero =
    row Background
        [ width fill, minHeight (px 100), verticalCenter, spacing 8, paddingBottom 8 ]
        [ wideNumberButton zero
        , buttonElement BackspacePressed "←"
        ]


romanButtomButtonRow : Value -> Element MyStyles Variation Msg
romanButtomButtonRow one =
    row Background
        [ width fill, minHeight (px 100), verticalCenter, spacing 8, paddingBottom 8 ]
        [ romanButton one
        , romanButtonElement BackspacePressed "←"
        ]


valueButtons : ConverterState -> Element MyStyles Variation Msg
valueButtons converterState =
    case Converter.Converter.values converterState of
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

        IntValues valueDict ->
            row None
                []
                [ column None
                    [ width fill, padding 8 ]
                    [ numberButtonRow valueDict.seven valueDict.eight valueDict.nine
                    , numberButtonRow valueDict.four valueDict.five valueDict.six
                    , numberButtonRow valueDict.one valueDict.two valueDict.three
                    , wideButtomButtonRow valueDict.zero
                    ]
                ]

        RomanValues valueDict ->
            row None
                []
                [ column None
                    [ width fill, padding 8 ]
                    [ romanButtonRow valueDict.d valueDict.m
                    , romanButtonRow valueDict.l valueDict.c
                    , romanButtonRow valueDict.v valueDict.x
                    , romanButtomButtonRow valueDict.i
                    ]
                ]


viewConverterSelection : ConverterState -> Element MyStyles Variation Msg
viewConverterSelection converterState =
    column None
        [ width fill ]
        (Converter.Converter.mapConverterNames viewConverter converterState)


viewInputs : ConverterState -> (String -> Element MyStyles Variation Msg) -> Element MyStyles Variation Msg
viewInputs converterState f =
    row None
        []
        [ mapInputNames f converterState
            |> column Background [ width fill ]
        ]


viewOutputs : ConverterState -> (String -> Element MyStyles Variation Msg) -> Element MyStyles Variation Msg
viewOutputs converterState f =
    row None
        []
        [ mapOutputNames f converterState
            |> column Background [ width fill ]
        ]


viewInputSelection : ConverterState -> Element MyStyles Variation Msg
viewInputSelection converterState =
    column None
        [ width fill ]
        [ InputSelectionMenu (converterName converterState) (outputName converterState)
            |> converterSelectionMenu
        , inputOutputRow converterState
        , viewInputUnit InputUnitChanged
            |> viewInputs converterState
        ]


viewOutputSelection : ConverterState -> Element MyStyles Variation Msg
viewOutputSelection converterState =
    column None
        [ width fill ]
        [ OutputSelectionMenu (converterName converterState) (inputName converterState)
            |> converterSelectionMenu
        , inputOutputRow converterState
        , viewInputUnit OutputUnitChanged
            |> viewOutputs converterState
        ]


viewConversion : ConverterState -> Element MyStyles Variation Msg
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


calculator : Model -> Element MyStyles Variation Msg
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
