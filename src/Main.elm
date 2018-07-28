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


type ButtonWidth
    = Quarter
    | Third
    | Half
    | TwoThirds
    | Whole


buttonWidth : ButtonWidth -> Float
buttonWidth width =
    case width of
        Quarter ->
            23

        Third ->
            31

        Half ->
            48

        TwoThirds ->
            64

        Whole ->
            98


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


buttonElement : ButtonWidth -> Bool -> Msg -> String -> Element MyStyles Variation Msg
buttonElement width enabled msg s =
    let
        commonAttributes =
            [ width |> buttonWidth |> percent |> minWidth
            , verticalCenter
            , Element.Events.onClick msg
            , minHeight (px 100)
            ]
    in
        case enabled of
            True ->
                Element.text s
                    |> Element.button ButtonStyle commonAttributes

            False ->
                Element.text s
                    |> Element.button
                        ButtonStyle
                        (commonAttributes
                            ++ [ attribute "disabled" "true"
                               , vary Disabled True
                               ]
                        )


valueButton : ButtonWidth -> Value -> Element MyStyles Variation Msg
valueButton width value =
    value
        |> Converter.Converter.toString
        |> buttonElement width (isActive value) (ValueButtonPressed value)


numberButtonRow : Value -> Value -> Value -> Element MyStyles Variation Msg
numberButtonRow first second third =
    buttonRow
        [ valueButton Third first
        , valueButton Third second
        , valueButton Third third
        ]


hexButtonRow : Value -> Value -> Value -> Value -> Element MyStyles Variation Msg
hexButtonRow first second third fourth =
    buttonRow
        [ valueButton Quarter first
        , valueButton Quarter second
        , valueButton Quarter third
        , valueButton Quarter fourth
        ]


romanButtonRow : Value -> Value -> Element MyStyles Variation Msg
romanButtonRow first second =
    buttonRow
        [ valueButton Half first
        , valueButton Half second
        ]


floatBottomRow : Value -> Value -> Element MyStyles Variation Msg
floatBottomRow comma zero =
    buttonRow
        [ valueButton Third comma
        , valueButton Third zero
        , buttonElement Third True BackspacePressed "←"
        ]


intBottomRow : Value -> Element MyStyles Variation Msg
intBottomRow zero =
    buttonRow
        [ valueButton TwoThirds zero
        , buttonElement Third True BackspacePressed "←"
        ]


romanButtomButtonRow : Value -> Element MyStyles Variation Msg
romanButtomButtonRow one =
    buttonRow
        [ valueButton Half one
        , buttonElement Half True BackspacePressed "←"
        ]


binaryButtonRow : Element MyStyles Variation Msg
binaryButtonRow =
    buttonRow
        [ buttonElement Whole True BackspacePressed "←"
        ]


buttonRow : List (Element MyStyles Variation Msg) -> Element MyStyles Variation Msg
buttonRow children =
    row Background
        [ width fill, minHeight (px 100), verticalCenter, spacing 8, paddingBottom 8 ]
        children


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
                    , floatBottomRow valueDict.transform valueDict.zero
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
                    , intBottomRow valueDict.zero
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

        BinaryValues valueDict ->
            row None
                []
                [ column None
                    [ width fill, padding 8 ]
                    [ romanButtonRow valueDict.zero valueDict.one
                    , binaryButtonRow
                    ]
                ]

        HexValues valueDict ->
            row None
                []
                [ column None
                    [ width fill, padding 8 ]
                    [ hexButtonRow valueDict.thirteen valueDict.fourteen valueDict.fifteen valueDict.zero
                    , hexButtonRow valueDict.nine valueDict.ten valueDict.eleven valueDict.twelve
                    , hexButtonRow valueDict.five valueDict.six valueDict.seven valueDict.eight
                    , hexButtonRow valueDict.one valueDict.two valueDict.three valueDict.four
                    , binaryButtonRow
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
