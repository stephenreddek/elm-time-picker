module Examples exposing (main)

import Html exposing (..)
import TimePicker exposing (TimePicker)


type Msg
    = DefaultTimePickerMsg TimePicker.Msg
    | SteppingTimePickerMsg TimePicker.Msg


type alias Model =
    { defaultTimePicker : TimePicker
    , steppingTimePicker : TimePicker
    }


init : ( Model, Cmd Msg )
init =
    ( Model (TimePicker.init Nothing) (TimePicker.init Nothing), Cmd.none )


steppingSettings : TimePicker.Settings
steppingSettings =
    let
        defaultSettings =
            TimePicker.defaultSettings
    in
        { defaultSettings | showSeconds = False, minuteStep = 15 }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DefaultTimePickerMsg msg ->
            let
                updatedPicker =
                    TimePicker.update TimePicker.defaultSettings msg model.defaultTimePicker
            in
                ( { model | defaultTimePicker = updatedPicker }, Cmd.none )

        SteppingTimePickerMsg msg ->
            let
                updatedPicker =
                    TimePicker.update steppingSettings msg model.steppingTimePicker
            in
                ( { model | steppingTimePicker = updatedPicker }, Cmd.none )


view : Model -> Html Msg
view { defaultTimePicker, steppingTimePicker } =
    div []
        [ div []
            [ h1 []
                [ text "Time Picker with defaults" ]
            , div []
                [ Html.map DefaultTimePickerMsg <| TimePicker.view TimePicker.defaultSettings defaultTimePicker ]
            , h1 []
                [ text "Time Picker without seconds and a minute step size of 15" ]
            , div []
                [ Html.map SteppingTimePickerMsg <| TimePicker.view steppingSettings steppingTimePicker ]
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
