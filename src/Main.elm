module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src, disabled, value)
import Html.Events exposing (..)
import List.Extra exposing (find)
import Converters exposing (..)


---- MODEL ----


type alias Test =
    {}


type alias Model =
    { converters : List Converter
    , valgtConverter : Maybe ConverterState
    }


type InputState
    = SiState SiDefinition (Maybe String)
    | FactorState FactorDefinition (Maybe String)
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
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = InputOppdatert String
    | SecondInputOppdatert String
    | MeassurableChanged String
    | InputUnitChanged String
    | OutputUnitChanged String


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


updatedInput : String -> InputState -> InputState
updatedInput inputString state =
    let
        inputFelt =
            if inputString == "" then
                Nothing
            else
                Just inputString
    in
        case state of
            SiState definition _ ->
                SiState definition inputFelt

            FactorState definition _ ->
                FactorState definition inputFelt

            ComboState definition tuple ->
                let
                    inputTuple =
                        createComboTuple inputString tuple
                in
                    ComboState definition inputTuple


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


majorInput : Maybe ConverterState -> Maybe String
majorInput converterState =
    case converterState of
        Just state ->
            case state.input of
                SiState _ input ->
                    input

                FactorState _ input ->
                    input

                ComboState _ tuple ->
                    Tuple.first tuple

        Nothing ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputOppdatert inputString ->
            case model.valgtConverter of
                Just converterState ->
                    ( { model | valgtConverter = Just { converterState | input = updatedInput inputString converterState.input } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

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
                                    , input = SiState converter.siUnit (majorInput model.valgtConverter)
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
                                        ( { model | valgtConverter = Just { converterState | input = SiState definition (majorInput model.valgtConverter) } }, Cmd.none )

                                    FactorUnit definition ->
                                        ( { model | valgtConverter = Just { converterState | input = FactorState definition (majorInput model.valgtConverter) } }, Cmd.none )

                                    ComboUnit definition ->
                                        ( { model | valgtConverter = Just { converterState | input = ComboState definition ( Nothing, Nothing ) } }, Cmd.none )

                            Nothing ->
                                ( { model | valgtConverter = Just { converterState | input = SiState converterState.converter.siUnit Nothing } }, Cmd.none )

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



---- VIEW ----


lagConverterOption : Converter -> Html Msg
lagConverterOption converter =
    option [] [ text converter.name ]


lagUnitOption unit =
    case unit of
        SiUnit unitName ->
            option [] [ text unitName.name ]

        FactorUnit definition ->
            option [] [ text definition.name.name ]

        ComboUnit definition ->
            option [] [ text definition.comboName ]


inputSelect converterValg =
    div []
        [ select [ onInput InputUnitChanged ] (List.map lagUnitOption ((SiUnit converterValg.converter.siUnit) :: converterValg.converter.factors))
        ]


outputSelect converterValg =
    div []
        [ select [ onInput OutputUnitChanged ] (List.map lagUnitOption ((SiUnit converterValg.converter.siUnit) :: converterValg.converter.factors))
        ]


makeConversion : ConverterState -> Float -> Float
makeConversion converterState input =
    case converterState.input of
        SiState _ inputMaybe ->
            case converterState.output of
                SiUnit _ ->
                    input

                FactorUnit definition ->
                    input / definition.factor

                ComboUnit definition ->
                    -4

        FactorState inputDefinition inputMaybe ->
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


convertInput : ConverterState -> Result String String
convertInput converterState =
    case converterState.input of
        SiState definition inputFelt ->
            let
                input =
                    parseInput inputFelt
            in
                case input of
                    Ok inputFloat ->
                        Ok (toString (makeConversion converterState inputFloat))

                    Err error ->
                        Err error

        FactorState definition inputFelt ->
            let
                input =
                    parseInput inputFelt
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


view : Model -> Html Msg
view model =
    div []
        (select [ onInput MeassurableChanged ]
            (option [] [ text "---------" ]
                :: List.map lagConverterOption model.converters
            )
            :: converterView model
        )


converterView : Model -> List (Html Msg)
converterView model =
    case model.valgtConverter of
        Maybe.Just converterState ->
            [ inputSelect converterState
            , inputField converterState
            , outputSelect converterState
            , outputDisplay converterState model
            ]

        Maybe.Nothing ->
            []


inputField : ConverterState -> Html Msg
inputField converterState =
    case converterState.input of
        SiState siDefinition stringMaybe ->
            input [ onInput InputOppdatert ] []

        FactorState factorDefinition stringMaybe ->
            input [ onInput InputOppdatert ] []

        ComboState comboDefinition stringStringMaybe ->
            div []
                [ input [ onInput InputOppdatert ] []
                , input [ onInput SecondInputOppdatert ] []
                ]


outputDisplay : ConverterState -> Model -> Html Msg
outputDisplay converterState model =
    let
        conversionResult =
            convertInput converterState
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
