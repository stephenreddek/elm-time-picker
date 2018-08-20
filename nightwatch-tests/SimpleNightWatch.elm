module SimpleNightwatch exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import TimePicker exposing (TimeEvent(..), TimePicker)


type Msg
    = TimePickerMsg TimePicker.Msg


type alias Model =
    { timePicker : TimePicker
    }


init : Model
init =
    Model (TimePicker.init Nothing)


update : Msg -> Model -> Model
update msg model =
    case msg of
        TimePickerMsg m ->
            let
                ( updatedPicker, timeEvent ) =
                    TimePicker.update TimePicker.defaultSettings m model.timePicker
            in
            { model | timePicker = updatedPicker }


view : Model -> Html Msg
view { timePicker } =
    div []
        [ div []
            [ h3 []
                [ text "Time Picker with defaults" ]
            , h4 [ id "selected-hours" ]
                [ TimePicker.selectedTime timePicker
                    |> Maybe.map (.hours >> String.fromInt)
                    |> Maybe.withDefault ""
                    |> text
                ]
            , h4 [ id "selected-minutes" ]
                [ TimePicker.selectedTime timePicker
                    |> Maybe.map (.minutes >> String.fromInt)
                    |> Maybe.withDefault ""
                    |> text
                ]
            , h4 [ id "selected-seconds" ]
                [ TimePicker.selectedTime timePicker
                    |> Maybe.map (.seconds >> String.fromInt)
                    |> Maybe.withDefault ""
                    |> text
                ]
            , div [ class "default-time-picker" ]
                [ Html.map TimePickerMsg <| TimePicker.view TimePicker.defaultSettings timePicker ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
