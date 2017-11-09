module SimpleNightwatch exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import TimePicker exposing (TimePicker, TimeEvent(..))


type Msg
    = TimePickerMsg TimePicker.Msg


type alias Model =
    { timePicker : TimePicker
    }


init : ( Model, Cmd Msg )
init =
    ( Model (TimePicker.init Nothing), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimePickerMsg msg ->
            let
                ( updatedPicker, timeEvent ) =
                    TimePicker.update TimePicker.defaultSettings msg model.timePicker
            in
                ( { model | timePicker = updatedPicker }, Cmd.none )


view : Model -> Html Msg
view { timePicker } =
    div []
        [ div []
            [ h3 []
                [ text "Time Picker with defaults" ]
            , h4 [ id "selected-hours" ]
                [ TimePicker.selectedTime timePicker
                    |> Maybe.map (toString << .hours)
                    |> Maybe.withDefault ""
                    |> text
                ]
            , h4 [ id "selected-minutes" ]
                [ TimePicker.selectedTime timePicker
                    |> Maybe.map (toString << .minutes)
                    |> Maybe.withDefault ""
                    |> text
                ]
            , h4 [ id "selected-seconds" ]
                [ TimePicker.selectedTime timePicker
                    |> Maybe.map (toString << .seconds)
                    |> Maybe.withDefault ""
                    |> text
                ]
            , div [ class "default-time-picker" ]
                [ Html.map TimePickerMsg <| TimePicker.view TimePicker.defaultSettings timePicker ]
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
