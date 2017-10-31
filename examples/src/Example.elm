module Examples exposing (main)

import Html exposing (..)
import TimePicker exposing (TimePicker, TimeEvent(..))


type Msg
    = DefaultTimePickerMsg TimePicker.Msg
    | SteppingTimePickerMsg TimePicker.Msg
    | PartiallyDisabledTimePickerMsg TimePicker.Msg


type alias Model =
    { defaultTimePicker : TimePicker
    , steppingTimePicker : TimePicker
    , partiallyDisabledTimePicker : TimePicker
    }


init : ( Model, Cmd Msg )
init =
    ( Model (TimePicker.init Nothing) (TimePicker.init Nothing) (TimePicker.init Nothing), Cmd.none )


steppingSettings : TimePicker.Settings
steppingSettings =
    let
        defaultSettings =
            TimePicker.defaultSettings
    in
        { defaultSettings | showSeconds = False, minuteStep = 15, use24Hours = True }


partiallyDisabledSettings : TimePicker.Settings
partiallyDisabledSettings =
    let
        defaultSettings =
            TimePicker.defaultSettings
    in
        { defaultSettings
            | isHourDisabled = (\value -> value % 2 == 0)
            , isMinuteDisabled = (\value -> value % 2 == 0)
            , isSecondDisabled = (\value -> value % 2 == 0)
            , isPeriodDisabled = (==) TimePicker.PM
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DefaultTimePickerMsg msg ->
            let
                ( updatedPicker, timeEvent ) =
                    TimePicker.update TimePicker.defaultSettings msg model.defaultTimePicker
            in
                ( { model | defaultTimePicker = updatedPicker }, Cmd.none )

        SteppingTimePickerMsg msg ->
            let
                ( updatedPicker, timeEvent ) =
                    TimePicker.update steppingSettings msg model.steppingTimePicker
            in
                ( { model | steppingTimePicker = updatedPicker }, Cmd.none )

        PartiallyDisabledTimePickerMsg msg ->
            let
                ( updatedPicker, timeEvent ) =
                    TimePicker.update partiallyDisabledSettings msg model.partiallyDisabledTimePicker
            in
                ( { model | partiallyDisabledTimePicker = updatedPicker }, Cmd.none )


view : Model -> Html Msg
view { defaultTimePicker, steppingTimePicker, partiallyDisabledTimePicker } =
    div []
        [ div []
            [ h1 []
                [ text "Time Picker with defaults" ]
            , div []
                [ Html.map DefaultTimePickerMsg <| TimePicker.view TimePicker.defaultSettings defaultTimePicker ]
            , h1 []
                [ text "Time Picker without seconds and a minute step size of 15 in 24-hour format" ]
            , div []
                [ Html.map SteppingTimePickerMsg <| TimePicker.view steppingSettings steppingTimePicker ]
            , h1 []
                [ text "Time Picker with even options disabled" ]
            , div []
                [ Html.map PartiallyDisabledTimePickerMsg <| TimePicker.view partiallyDisabledSettings partiallyDisabledTimePicker ]
            ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
