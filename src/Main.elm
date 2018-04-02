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
    , inputFelt : Maybe String
    }


type InputState
    = SiState SiDefinition (Maybe String)
    | FactorState FactorDefinition (Maybe String)
    | ComboState ComboDefinition (Maybe ( String, String ))


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
      , inputFelt = Maybe.Nothing
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputOppdatert inputString ->
            if inputString == "" then
                ( { model | inputFelt = Maybe.Nothing }, Cmd.none )
            else
                ( { model | inputFelt = Maybe.Just inputString }, Cmd.none )

        SecondInputOppdatert inputString ->
            if inputString == "" then
                ( { model | inputFelt = Maybe.Nothing }, Cmd.none )
            else
                ( { model | inputFelt = Maybe.Just inputString }, Cmd.none )

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
                                    , input = SiState converter.siUnit (Just "")
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
                                        ( { model | valgtConverter = Just { converterState | input = SiState definition (Just "") } }, Cmd.none )

                                    FactorUnit definition ->
                                        ( { model | valgtConverter = Just { converterState | input = FactorState definition (Just "") } }, Cmd.none )

                                    ComboUnit definition ->
                                        ( { model | valgtConverter = Just { converterState | input = ComboState definition (Just ( "", "" )) } }, Cmd.none )

                            Nothing ->
                                ( { model | valgtConverter = Just { converterState | input = SiState converterState.converter.siUnit (Just "") } }, Cmd.none )

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
            1


convertInput : ConverterState -> Maybe String -> Result String String
convertInput converterState inputFelt =
    case inputFelt of
        Just inputString ->
            let
                input =
                    String.toFloat inputString
            in
                case input of
                    Ok inputFloat ->
                        Ok (toString (makeConversion converterState inputFloat))

                    Err _ ->
                        Err (inputString ++ " er ikke et tall")

        Nothing ->
            Ok ""


view : Model -> Html Msg
view model =
    div []
        [ select [ onInput MeassurableChanged ]
            (option [] [ text "---------" ]
                :: List.map lagConverterOption model.converters
            )
        , case model.valgtConverter of
            Maybe.Just converterValg ->
                inputSelect converterValg

            Maybe.Nothing ->
                div [] []
        , case model.valgtConverter of
            Maybe.Just converterValg ->
                div [] [ input [ onInput InputOppdatert ] [] ]

            Maybe.Nothing ->
                div [] []
        , case model.valgtConverter of
            Maybe.Just converterValg ->
                outputSelect converterValg

            Maybe.Nothing ->
                div [] []
        , case model.valgtConverter of
            Maybe.Just converterValg ->
                let
                    conversionResult =
                        convertInput converterValg model.inputFelt
                in
                    case conversionResult of
                        Ok res ->
                            div [] [ input [ disabled True, value res ] [] ]

                        Err error ->
                            div [] [ input [ disabled True, value error ] [] ]

            Maybe.Nothing ->
                div [] []
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
