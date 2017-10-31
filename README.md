# elm-time-picker

```shell
elm package install stephenreddek/elm-time-picker
```

## Usage

The `TimePicker.init` function creates a time picker. If there's a value provided, it will be used as the default value.

```elm

    { noDefaultPicker = TimePicker.init Nothing
    , defaultedPicker = TimePicker.init
        (Just
            { hours = 17
            , minutes = 30
            , seconds = 0
            }
        )
    }
```

Options about how to display the time picker are passed into the `view` and `update` function calls. You can create the record one time and reference it in each call or have modified settings for different calls.

```elm
timePickerSettings : TimePicker.Settings
timePickerSettings =
    let
        defaultSettings =
            TimePicker.defaultSettings
    in
        { defaultSettings | showSeconds = False, minuteStep = 15, use24Hours = True }
```

Handle messages to the picker in your update function. The TimePicker's update function also communicates when the value of the time picker has changed.
```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg { timePicker } =
    case msg of
        TimePickerMsg msg ->
            let
                ( updatedModel, timeEvent ) =
                    TimePicker.update timePickerSettings msg timePicker

                _ =
                    case timeEvent of
                        NoChange ->
                            Nothing

                        Changed time ->
                            Debug.log "The value was changed to " time
            in
                ( Model updatedModel, Cmd.none )
```

To render the time picker, call the view function
```elm
Html.map TimePickerMsg <| TimePicker.view timePicker
```

Access the selected time at any time by using the `selectedTime` function.
```elm
time =
    TimePicker.selectedTime model.timePicker
```

## Example

Checkout the [example](https://github.com/stephenreddek/elm-time-picker/tree/master/examples/README "elm-time-picker example") to test it or see an example of how to wire it up.

## Css

An example style for the time picker can be found [in the project repo](https://github.com/stephenreddek/elm-time-picker/tree/master/css "elm-time-picker Github"). You can use the styles provided or create your own.