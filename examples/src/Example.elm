module Example exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import TimePicker exposing (TimeEvent(..), TimePicker)


type Msg
    = DefaultTimePickerMsg TimePicker.Msg
    | SteppingTimePickerMsg TimePicker.Msg
    | PartiallyDisabledTimePickerMsg TimePicker.Msg


type alias Model =
    { defaultTimePicker : TimePicker
    , steppingTimePicker : TimePicker
    , partiallyDisabledTimePicker : TimePicker
    }


init : Model
init =
    Model (TimePicker.init Nothing) (TimePicker.init Nothing) (TimePicker.init Nothing)


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
        | isHourDisabled = \value -> modBy 2 value == 0
        , isMinuteDisabled = \value -> modBy 2 value == 0
        , isSecondDisabled = \value -> modBy 2 value == 0
        , isPeriodDisabled = (==) TimePicker.PM
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        DefaultTimePickerMsg m ->
            let
                ( updatedPicker, timeEvent ) =
                    TimePicker.update TimePicker.defaultSettings m model.defaultTimePicker
            in
            { model | defaultTimePicker = updatedPicker }

        SteppingTimePickerMsg m ->
            let
                ( updatedPicker, timeEvent ) =
                    TimePicker.update steppingSettings m model.steppingTimePicker
            in
            { model | steppingTimePicker = updatedPicker }

        PartiallyDisabledTimePickerMsg m ->
            let
                ( updatedPicker, timeEvent ) =
                    TimePicker.update partiallyDisabledSettings m model.partiallyDisabledTimePicker
            in
            { model | partiallyDisabledTimePicker = updatedPicker }


view : Model -> Html Msg
view { defaultTimePicker, steppingTimePicker, partiallyDisabledTimePicker } =
    div []
        [ div []
            [ h3 []
                [ text "Time Picker with defaults" ]
            , div [ class "default-time-picker" ]
                [ Html.map DefaultTimePickerMsg <| TimePicker.view TimePicker.defaultSettings defaultTimePicker ]
            , h3 []
                [ text "Time Picker without seconds and a minute step size of 15 in 24-hour format" ]
            , div [ class "default-time-picker" ]
                [ Html.map SteppingTimePickerMsg <| TimePicker.view steppingSettings steppingTimePicker ]
            , h3 []
                [ text "Time Picker with even options disabled and opens upwards" ]
            , div [ class "bootstrap-time-picker" ]
                [ Html.map PartiallyDisabledTimePickerMsg <| TimePicker.view partiallyDisabledSettings partiallyDisabledTimePicker ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
