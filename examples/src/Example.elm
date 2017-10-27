module Examples exposing (main)

import Html exposing (..)
import TimePicker exposing (TimePicker)


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
                updatedPicker =
                    TimePicker.update TimePicker.defaultSettings msg model.timePicker
            in
                ( Model updatedPicker, Cmd.none )


view : Model -> Html Msg
view { timePicker } =
    div []
        [ div []
            [ h1 []
                [ text "Time Picker with defaults" ]
            , div []
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
