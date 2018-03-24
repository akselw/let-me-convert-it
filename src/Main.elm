module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src, disabled, value)
import Html.Events exposing (..)
import List.Extra exposing (find)


---- MODEL ----


type alias Model =
    { converters : List Converter
    , valgtConverter :
        Maybe ConverterSelection
    , inputFelt : Maybe String
    }


type alias ConverterSelection =
    { converter : Converter
    , input : Unit
    , output : Unit
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


type alias Converter =
    { name : String
    , siUnit : Unit
    , factors : List Unit
    }


erTall : (Int -> Int) -> (String -> Result String String)
erTall func =
    \inp ->
        case String.toInt inp of
            Ok n ->
                Ok (toString (func n))

            Err e ->
                Err e


converter : Converter
converter =
    { name = "Avstand"
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
        ]
    }


init : ( Model, Cmd Msg )
init =
    ( { converters =
            [ converter ]
      , valgtConverter = Maybe.Nothing
      , inputFelt = Maybe.Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = InputOppdatert String
    | MeassurableChanged String
    | InputUnitChanged String
    | OutputUnitChanged String


sameUnitName : String -> (Unit -> Bool)
sameUnitName valgtNavn =
    \unit ->
        case unit of
            SiUnit unitName ->
                valgtNavn == unitName.name

            FactorUnit _ unitName ->
                valgtNavn == unitName.name


findUnit : String -> ConverterSelection -> Maybe Unit
findUnit valgtNavn converterSelection =
    find (sameUnitName valgtNavn) (converterSelection.converter.siUnit :: converterSelection.converter.factors)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputOppdatert inputString ->
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
                                    , input = converter.siUnit
                                    , output = converter.siUnit
                                    }
                          }
                        , Cmd.none
                        )

                    Maybe.Nothing ->
                        ( { model | valgtConverter = Maybe.Nothing }, Cmd.none )

        InputUnitChanged inputName ->
            case model.valgtConverter of
                Just converterSelection ->
                    let
                        input =
                            findUnit inputName converterSelection
                    in
                        case input of
                            Just chosenUnit ->
                                ( { model | valgtConverter = Just { converterSelection | input = chosenUnit } }, Cmd.none )

                            Nothing ->
                                ( { model | valgtConverter = Just { converterSelection | input = converterSelection.converter.siUnit } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        OutputUnitChanged outputName ->
            case model.valgtConverter of
                Just converterSelection ->
                    let
                        output =
                            findUnit outputName converterSelection
                    in
                        case output of
                            Just chosenUnit ->
                                ( { model | valgtConverter = Just { converterSelection | output = chosenUnit } }, Cmd.none )

                            Nothing ->
                                ( { model | valgtConverter = Just { converterSelection | output = converterSelection.converter.siUnit } }, Cmd.none )

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

        FactorUnit _ unitName ->
            option [] [ text unitName.name ]


inputSelect converterValg =
    div []
        [ select [ onInput InputUnitChanged ] (List.map lagUnitOption (converterValg.converter.siUnit :: converterValg.converter.factors))
        ]


outputSelect converterValg =
    div []
        [ select [ onInput OutputUnitChanged ] (List.map lagUnitOption (converterValg.converter.siUnit :: converterValg.converter.factors))
        ]


makeConversion : ConverterSelection -> Float -> Float
makeConversion converterSelection input =
    case converterSelection.input of
        SiUnit _ ->
            case converterSelection.output of
                SiUnit _ ->
                    input

                FactorUnit factor _ ->
                    input / factor

        FactorUnit factor _ ->
            case converterSelection.output of
                SiUnit _ ->
                    input * factor

                FactorUnit outFactor _ ->
                    input * factor / outFactor


convertInput : ConverterSelection -> Maybe String -> Result String String
convertInput converterSelection inputFelt =
    case inputFelt of
        Just inputString ->
            let
                input =
                    String.toFloat inputString
            in
                case input of
                    Ok inputFloat ->
                        Ok (toString (makeConversion converterSelection inputFloat))

                    Err _ ->
                        Err (inputString ++ " er ikke et tall")

        Nothing ->
            Ok ""


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , div []
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
        ]



-- case model.valgtConverter of
-- Maybe.Just converter ->
--     div []
--         [ img [ src "/logo.svg" ] []
--         , h1 [] [ text converter.navn ]
--         , div [] [ div [] [ text "Fra" ], input [ onInput InputOppdatert ] [] ]
--         , div []
--             [ div [] [ text "Til" ]
--             , resultat converter model.inputFelt
--             ]
--         , div [] [ text "" ]
--         ]
--
-- Maybe.Nothing ->
--     div []
--         [ img [ src "/logo.svg" ] []
--         , h1 [] [ text "Velg en konverter" ]
--         , ul []
--             (List.map (\c -> li [] [ text c.navn ]) model.converters)
--         ]
-- resultat : Converter -> Maybe String -> Html Msg
-- resultat converter inputFelt =
--     case inputFelt of
--         Maybe.Just inp ->
--             let
--                 res =
--                     converter.funksjon inp
--             in
--                 case res of
--                     Ok str ->
--                         input [ disabled True, value str ] []
--
--                     Err error ->
--                         input [ disabled True, value ("Error: " ++ error) ] []
--
--         Maybe.Nothing ->
--             input [ disabled True ] []
---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
