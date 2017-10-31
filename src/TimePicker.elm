module TimePicker exposing (Time, TimePicker, Settings, Period(..), Msg, defaultSettings, selectedTime, init, update, view)

{-| A time picker in pure elm.

# Models
@docs Time, TimePicker, Settings, Period, defaultSettings, selectedTime

# Update
@docs Msg, init, update

# View
@docs view

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode


{-| The base model for the time picker
-}
type TimePicker
    = TimePicker Model


{-| The base way to represent time
 Note: Hours are counted in 24-hour format from midnight at 0
-}
type alias Time =
    { hours : Int
    , minutes : Int
    , seconds : Int
    }


{-| Contains the configuration that doesn't need to be maintained by the library
-}
type alias Settings =
    { showHours : Bool
    , showMinutes : Bool
    , showSeconds : Bool
    , use24Hours : Bool
    , placeholder : String
    , hourStep : Int
    , minuteStep : Int
    , secondStep : Int
    , disabled : Bool
    , hideDisabledOptions : Bool
    , isHourDisabled : Int -> Bool
    , isMinuteDisabled : Int -> Bool
    , isSecondDisabled : Int -> Bool
    , isPeriodDisabled : Period -> Bool
    }


{-| Period denotes whether its AM or PM when using 12-hour format
-}
type Period
    = AM
    | PM


type alias Model =
    { open : Bool
    , value : Maybe Time
    }


{-| The internal messages that the picker uses to operate
-}
type Msg
    = Clear
    | Focus
    | Blur
    | SelectHour Int
    | SelectMinute Int
    | SelectSecond Int
    | SelectPeriod Period
    | NoOp


{-| The basic configuration for a TimePicker
-}
defaultSettings : Settings
defaultSettings =
    { showHours = True
    , showMinutes = True
    , showSeconds = True
    , use24Hours = False
    , placeholder = "Select time"
    , hourStep = 1
    , minuteStep = 1
    , secondStep = 1
    , disabled = False
    , hideDisabledOptions = False
    , isHourDisabled = always False
    , isMinuteDisabled = always False
    , isSecondDisabled = always False
    , isPeriodDisabled = always False
    }


{-| Returns the current value of the time picker
-}
selectedTime : TimePicker -> Maybe Time
selectedTime (TimePicker { value }) =
    value


{-| Function for initializing a closed and empty TimePicker
-}
init : Maybe Time -> TimePicker
init initialValue =
    TimePicker
        { open = False
        , value = Nothing
        }


defaultTime : Time
defaultTime =
    { hours = 0
    , minutes = 0
    , seconds = 0
    }


{-| Function to update the model when messages come
-}
update : Settings -> Msg -> TimePicker -> TimePicker
update settings msg (TimePicker ({ value } as model)) =
    case msg of
        Clear ->
            TimePicker { model | open = False, value = Nothing }

        Focus ->
            TimePicker { model | open = True }

        Blur ->
            TimePicker { model | open = False }

        SelectHour hours ->
            let
                timeToUpdate =
                    Maybe.withDefault defaultTime value
            in
                TimePicker { model | value = Just { timeToUpdate | hours = hours } }

        SelectMinute minutes ->
            let
                timeToUpdate =
                    Maybe.withDefault defaultTime value
            in
                TimePicker { model | value = Just { timeToUpdate | minutes = minutes } }

        SelectSecond seconds ->
            let
                timeToUpdate =
                    Maybe.withDefault defaultTime value
            in
                TimePicker { model | value = Just { timeToUpdate | seconds = seconds } }

        SelectPeriod period ->
            let
                timeToUpdate =
                    Maybe.withDefault defaultTime value
            in
                case period of
                    AM ->
                        if timeToUpdate.hours >= 12 then
                            TimePicker { model | value = Just { timeToUpdate | hours = timeToUpdate.hours - 12 } }
                        else
                            TimePicker { model | value = Just timeToUpdate }

                    PM ->
                        if timeToUpdate.hours >= 12 then
                            TimePicker { model | value = Just timeToUpdate }
                        else
                            TimePicker { model | value = Just { timeToUpdate | hours = timeToUpdate.hours + 12 } }

        NoOp ->
            TimePicker model


cssPrefix : String
cssPrefix =
    "elm-time-picker-"


onWithoutLosingFocus : String -> Msg -> Html.Attribute Msg
onWithoutLosingFocus eventName =
    Json.Decode.succeed
        >> onWithOptions eventName
            { preventDefault = True
            , stopPropagation = True
            }


{-| Function for viewing the time picker
-}
view : Settings -> TimePicker -> Html Msg
view settings (TimePicker model) =
    let
        optionsDisplay =
            if model.open && not settings.disabled then
                [ viewDropDown settings model ]
            else
                []

        inputValue =
            model.value
                |> Maybe.map (formatValue settings >> Html.Attributes.value >> List.singleton)
                |> Maybe.withDefault []

        clearButton =
            model.value
                |> Maybe.map (\_ -> [ a [ class (cssPrefix ++ "panel-clear-btn"), href "javascript:void(0);", onWithoutLosingFocus "mousedown" NoOp, onWithoutLosingFocus "mouseup" NoOp, onClick Clear ] [] ])
                |> Maybe.withDefault []
    in
        div [ classList [ ( cssPrefix ++ "container", True ), ( cssPrefix ++ "active", model.open ) ] ]
            [ div [ class (cssPrefix ++ "inner-container") ] <|
                [ div [ class (cssPrefix ++ "input-container") ] <|
                    [ input ([ type_ "text", onFocus Focus, onBlur Blur, placeholder settings.placeholder, readonly True, disabled settings.disabled ] ++ inputValue) [] ]
                        ++ clearButton
                ]
                    ++ optionsDisplay
            ]


formatValue : Settings -> Time -> String
formatValue settings time =
    let
        hoursDisplay time =
            if settings.showHours then
                [ hourFormatter settings time.hours ]
            else
                []

        minutesDisplay time =
            if settings.showMinutes then
                [ paddedFormatter time.minutes ]
            else
                []

        secondsDisplay time =
            if settings.showSeconds then
                [ paddedFormatter time.seconds ]
            else
                []

        timePartsDisplay time =
            [ hoursDisplay time, minutesDisplay time, secondsDisplay time ]
                |> List.concat
                |> String.join ":"

        periodDisplay time =
            if settings.showHours && not settings.use24Hours then
                if isInAM (Just time) then
                    " AM"
                else
                    " PM"
            else
                ""
    in
        timePartsDisplay time ++ periodDisplay time


viewDropDown : Settings -> Model -> Html Msg
viewDropDown settings model =
    let
        selectionOption : String -> Bool -> Bool -> Msg -> Html Msg
        selectionOption valueText isSelected isDisabled msg =
            let
                optionalClick =
                    if isDisabled then
                        []
                    else
                        [ onClick msg ]
            in
                li ([ classList [ ( "elm-time-picker-panel-select-option-selected", isSelected ), ( "elm-time-picker-panel-select-option-disabled", isDisabled ) ] ] ++ optionalClick)
                    [ text valueText ]

        toOption : (a -> String) -> (Time -> a) -> (a -> Bool) -> (a -> Msg) -> a -> Html Msg
        toOption formatter accessor isDisabledValue toMsg value =
            let
                isSelected =
                    Maybe.map accessor model.value == Just value
            in
                selectionOption (formatter value) isSelected (isDisabledValue value) (toMsg value)

        steppingRange step minVal maxVal =
            List.range minVal (maxVal // step)
                |> List.map (\val -> val * step)

        hours =
            if settings.use24Hours then
                steppingRange settings.hourStep 0 23
            else if isInPM model.value then
                steppingRange settings.hourStep 12 23
            else
                steppingRange settings.hourStep 0 11

        minutes =
            steppingRange settings.minuteStep 0 59

        seconds =
            steppingRange settings.secondStep 0 59

        hourOptions =
            if settings.showHours then
                [ div [ class (cssPrefix ++ "panel-select") ]
                    [ ul []
                        (hours
                            |> List.filter (\hour -> not settings.hideDisabledOptions || not (settings.isHourDisabled hour))
                            |> List.map (toOption (hourFormatter settings) .hours settings.isHourDisabled SelectHour)
                        )
                    ]
                ]
            else
                []

        minuteOptions =
            if settings.showMinutes then
                [ div [ class (cssPrefix ++ "panel-select") ]
                    [ ul []
                        (minutes
                            |> List.filter (\minute -> not settings.hideDisabledOptions || not (settings.isMinuteDisabled minute))
                            |> List.map (toOption paddedFormatter .minutes settings.isMinuteDisabled SelectMinute)
                        )
                    ]
                ]
            else
                []

        secondOptions =
            if settings.showSeconds then
                [ div [ class (cssPrefix ++ "panel-select") ]
                    [ ul []
                        (seconds
                            |> List.filter (\second -> not settings.hideDisabledOptions || not (settings.isSecondDisabled second))
                            |> List.map (toOption paddedFormatter .seconds settings.isSecondDisabled SelectSecond)
                        )
                    ]
                ]
            else
                []

        periodSelectionOption period =
            if not settings.hideDisabledOptions || not (settings.isPeriodDisabled period) then
                [ selectionOption (toString period) (isInPeriod period model) (settings.isPeriodDisabled period) (SelectPeriod period) ]
            else
                []

        periodOptions =
            if not settings.use24Hours && settings.showHours then
                [ div [ class (cssPrefix ++ "panel-select") ]
                    [ ul [] <|
                        periodSelectionOption AM
                            ++ periodSelectionOption PM
                    ]
                ]
            else
                []
    in
        div [ class (cssPrefix ++ "panel-combobox"), onWithoutLosingFocus "mousedown" NoOp, onWithoutLosingFocus "mouseup" NoOp ] <|
            hourOptions
                ++ minuteOptions
                ++ secondOptions
                ++ periodOptions


paddedFormatter : Int -> String
paddedFormatter value =
    if value < 10 then
        "0" ++ toString value
    else
        toString value


hourFormatter : Settings -> Int -> String
hourFormatter settings value =
    if settings.use24Hours then
        paddedFormatter value
    else if value == 0 then
        "12"
    else if value > 12 then
        toString (value - 12)
    else
        toString value


isInAM : Maybe Time -> Bool
isInAM value =
    value
        |> Maybe.map (.hours >> (\hours -> hours < 12))
        |> Maybe.withDefault False


isInPM : Maybe Time -> Bool
isInPM value =
    value
        |> Maybe.map (.hours >> (\hours -> hours >= 12))
        |> Maybe.withDefault False


isInPeriod : Period -> Model -> Bool
isInPeriod period model =
    case period of
        AM ->
            isInAM model.value

        PM ->
            isInPM model.value
