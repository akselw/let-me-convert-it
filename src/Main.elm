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
    , valgtConverter : Maybe ConverterState
    , input : String
    }


type InputState
    = SiState SiDefinition
    | FactorState FactorDefinition
    | ComboState ComboDefinition ( Maybe String, Maybe String )


type alias ConverterState =
    { converter : Converter
    , input : InputState
    , output : Unit
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
      , valgtConverter = Maybe.Nothing
      , input = "0"
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SecondInputOppdatert String
    | MeassurableChanged String
    | InputUnitChanged String
    | OutputUnitChanged String
    | NumberPressed String
    | CommaPressed
    | BackspacePressed


sameUnitName : String -> (Unit -> Bool)
sameUnitName valgtNavn =
    \unit ->
        case unit of
            SiUnit unitName ->
                valgtNavn == unitName.name

            FactorUnit definition ->
                valgtNavn == definition.name.name

            ComboUnit definition ->
                valgtNavn == definition.comboName


findUnit : String -> ConverterState -> Maybe Unit
findUnit valgtNavn converterState =
    find (sameUnitName valgtNavn) ((SiUnit converterState.converter.siUnit) :: converterState.converter.factors)


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


updatedMinorInput : String -> ComboDefinition -> ( Maybe String, Maybe String ) -> InputState
updatedMinorInput inputString definition tuple =
    let
        inputFelt =
            if inputString == "" then
                Nothing
            else
                Just inputString

        newTuple =
            ( Tuple.first tuple, inputFelt )
    in
        ComboState definition newTuple


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SecondInputOppdatert inputString ->
            case model.valgtConverter of
                Just converterState ->
                    case converterState.input of
                        ComboState definition tuple ->
                            ( { model | valgtConverter = Just { converterState | input = updatedMinorInput inputString definition tuple } }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        MeassurableChanged meassurableName ->
            let
                converter =
                    find (\a -> a.name == meassurableName) model.converters
            in
                case converter of
                    Maybe.Just converter ->
                        ( { model
                            | valgtConverter =
                                Maybe.Just
                                    { converter = converter
                                    , input = SiState converter.siUnit
                                    , output = SiUnit converter.siUnit
                                    }
                          }
                        , Cmd.none
                        )

                    Maybe.Nothing ->
                        ( { model | valgtConverter = Maybe.Nothing }, Cmd.none )

        InputUnitChanged inputName ->
            case model.valgtConverter of
                Just converterState ->
                    let
                        input =
                            findUnit inputName converterState
                    in
                        case input of
                            Just chosenUnit ->
                                case chosenUnit of
                                    SiUnit definition ->
                                        ( { model | valgtConverter = Just { converterState | input = SiState definition } }, Cmd.none )

                                    FactorUnit definition ->
                                        ( { model | valgtConverter = Just { converterState | input = FactorState definition } }, Cmd.none )

                                    ComboUnit definition ->
                                        ( { model | valgtConverter = Just { converterState | input = ComboState definition ( Nothing, Nothing ) } }, Cmd.none )

                            Nothing ->
                                ( { model | valgtConverter = Just { converterState | input = SiState converterState.converter.siUnit } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        OutputUnitChanged outputName ->
            case model.valgtConverter of
                Just converterState ->
                    let
                        output =
                            findUnit outputName converterState
                    in
                        case output of
                            Just chosenUnit ->
                                ( { model | valgtConverter = Just { converterState | output = chosenUnit } }, Cmd.none )

                            Nothing ->
                                ( { model | valgtConverter = Just { converterState | output = SiUnit converterState.converter.siUnit } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NumberPressed numberString ->
            if model.input == "0" then
                ( { model | input = numberString }, Cmd.none )
            else
                ( { model | input = model.input ++ numberString }, Cmd.none )

        CommaPressed ->
            if String.contains "." model.input then
                ( model, Cmd.none )
            else
                ( { model | input = model.input ++ "." }, Cmd.none )

        BackspacePressed ->
            if String.length model.input == 1 then
                ( { model | input = "0" }, Cmd.none )
            else
                ( { model | input = String.dropRight 1 model.input }, Cmd.none )



---- VIEW ----


lagConverterOption : Converter -> Html Msg
lagConverterOption converter =
    option [] [ Html.text converter.name ]


lagUnitOption unit =
    case unit of
        SiUnit unitName ->
            option [] [ Html.text unitName.name ]

        FactorUnit definition ->
            option [] [ Html.text definition.name.name ]

        ComboUnit definition ->
            option [] [ Html.text definition.comboName ]


inputSelect converterValg =
    div []
        [ select [ Html.Events.onInput InputUnitChanged ] (List.map lagUnitOption ((SiUnit converterValg.converter.siUnit) :: converterValg.converter.factors))
        ]


outputSelect converterValg =
    div []
        [ select [ Html.Events.onInput OutputUnitChanged ] (List.map lagUnitOption ((SiUnit converterValg.converter.siUnit) :: converterValg.converter.factors))
        ]


makeConversion : ConverterState -> Float -> Float
makeConversion converterState input =
    case converterState.input of
        SiState _ ->
            case converterState.output of
                SiUnit _ ->
                    input

                FactorUnit definition ->
                    input / definition.factor

                ComboUnit definition ->
                    -4

        FactorState inputDefinition ->
            case converterState.output of
                SiUnit _ ->
                    input * inputDefinition.factor

                FactorUnit outputDefinition ->
                    input * inputDefinition.factor / outputDefinition.factor

                ComboUnit definition ->
                    -4

        ComboState definition inputMaybe ->
            -4


makeComboConversion : Float -> Float -> Unit -> ( Float, Float ) -> Float
makeComboConversion majorFactor minorFactor outputUnit inputTuple =
    let
        siSum : Float
        siSum =
            Tuple.first inputTuple * majorFactor + Tuple.second inputTuple * minorFactor
    in
        case outputUnit of
            SiUnit _ ->
                siSum

            FactorUnit definition ->
                siSum / definition.factor

            ComboUnit comboDefinition ->
                -1


parseInput : Maybe String -> Result String Float
parseInput inputMaybe =
    case inputMaybe of
        Just inputString ->
            let
                inputRes =
                    String.toFloat inputString
            in
                case inputRes of
                    Ok inputFloat ->
                        Ok inputFloat

                    Err _ ->
                        Err (inputString ++ " er ikke et tall")

        Nothing ->
            Ok 0


removeFormatting : String -> String
removeFormatting input =
    input |> String.split " " |> String.join ""


convertInput : ConverterState -> String -> Result String String
convertInput converterState inputFelt =
    case converterState.input of
        SiState definition ->
            let
                input =
                    inputFelt |> removeFormatting |> Just |> parseInput
            in
                case input of
                    Ok inputFloat ->
                        Ok (toString (makeConversion converterState inputFloat))

                    Err error ->
                        Err error

        FactorState definition ->
            let
                input =
                    parseInput (Just inputFelt)
            in
                case input of
                    Ok inputFloat ->
                        Ok (toString (makeConversion converterState inputFloat))

                    Err error ->
                        Err error

        ComboState definition tuple ->
            let
                majorInput =
                    parseInput (Tuple.first tuple)

                minorInput =
                    parseInput (Tuple.second tuple)
            in
                case ( majorInput, minorInput ) of
                    ( Ok major, Ok minor ) ->
                        Ok (toString (makeComboConversion definition.majorFactor definition.minorFactor converterState.output ( major, minor )))

                    _ ->
                        Err "Feil"


type MyStyles
    = None
    | InputStyle
    | ButtonStyle



-- We define our stylesheet


stylesheet =
    Style.styleSheet
        [ Style.style None []
        , Style.style InputStyle
            [ Color.text black
            , Color.background lightGray
            , Font.size 20 -- all units given as px
            ]
        , Style.style ButtonStyle
            [ Color.text white
            , Color.background darkGray
            , Font.size 20 -- all units given as px
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


calc : Model -> Element MyStyles variation Msg
calc model =
    let
        outputBox =
            case model.valgtConverter of
                Just converterState ->
                    let
                        conversionResult =
                            convertInput converterState model.input
                    in
                        case conversionResult of
                            Ok res ->
                                el InputStyle [ width (px 300) ] (Element.text res)

                            Err error ->
                                el InputStyle [ width (px 300) ] (Element.text "")

                Nothing ->
                    el InputStyle [ width (px 300) ] (Element.text "")
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
                            , content = el InputStyle [ Element.Events.onClick CommaPressed, width (percent 100), Element.Attributes.center ] (Element.text "meter")
                            }
                       , cell
                            { start = ( 2, 0 )
                            , width = 2
                            , height = 1
                            , content = el InputStyle [ Element.Events.onClick CommaPressed, width (percent 100), Element.Attributes.center ] (Element.text "feet")
                            }
                       , cell
                            { start = ( 0, 1 )
                            , width = 4
                            , height = 1
                            , content = el InputStyle [ Element.Events.onClick CommaPressed, width (percent 100), Element.Attributes.center ] (Element.text model.input)
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
                            , content = el ButtonStyle [ Element.Events.onClick CommaPressed ] (Element.text ",")
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


calculator : Model -> Html Msg
calculator model =
    Element.layout stylesheet <|
        -- An el is the most basic element, like a <div>
        calc model



-- Element.layout renders the elements as html.
-- Every layout requires a stylesheet.


unitSelector : Model -> Html Msg
unitSelector model =
    div []
        (select [ Html.Events.onInput MeassurableChanged ]
            (option [] [ Html.text "---------" ]
                :: List.map lagConverterOption model.converters
            )
            :: converterView model
        )


view : Model -> Html Msg
view model =
    div []
        [ unitSelector model
        , calculator model
        ]


converterView : Model -> List (Html Msg)
converterView model =
    case model.valgtConverter of
        Maybe.Just converterState ->
            [ inputSelect converterState
            , outputSelect converterState
            , outputDisplay converterState model
            ]

        Maybe.Nothing ->
            []


outputDisplay : ConverterState -> Model -> Html Msg
outputDisplay converterState model =
    let
        conversionResult =
            convertInput converterState model.input
    in
        case conversionResult of
            Ok res ->
                div [] [ input [ disabled True, value res ] [] ]

            Err error ->
                div [] [ input [ disabled True, value error ] [] ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
