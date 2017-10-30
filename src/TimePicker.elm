module TimePicker exposing (Time, Period, TimePicker, Settings, Msg, defaultSettings, init, update, view)

{-| A time picker in pure elm.

# Models
@docs Time, Period, TimePicker, Settings, defaultSettings

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


{-| Represents AM and PM for a time
-}
type Period
    = AM
    | PM


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
    }


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
    }


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
                if period == AM then
                    if timeToUpdate.hours >= 12 then
                        TimePicker { model | value = Just { timeToUpdate | hours = timeToUpdate.hours - 12 } }
                    else
                        TimePicker { model | value = Just timeToUpdate }
                else if timeToUpdate.hours >= 12 then
                    TimePicker { model | value = Just timeToUpdate }
                else
                    TimePicker { model | value = Just { timeToUpdate | hours = timeToUpdate.hours + 12 } }

        NoOp ->
            TimePicker model


cssPrefix : String
cssPrefix =
    "elm-time-picker-"


{-| Function for viewing the time picker
-}
view : Settings -> TimePicker -> Html Msg
view settings (TimePicker model) =
    let
        steppingRange step maxVal =
            List.range 0 (maxVal // step)
                |> List.map (\val -> val * step)

        maxHours =
            if settings.use24Hours then
                23
            else
                11

        hours =
            if isInAM then
                steppingRange settings.hourStep maxHours
            else
                maxHours
                    |> steppingRange settings.hourStep
                    |> List.map ((+) 12)

        minutes =
            steppingRange settings.minuteStep 59

        seconds =
            steppingRange settings.secondStep 59

        paddedFormatter value =
            if value < 10 then
                "0" ++ toString value
            else
                toString value

        hourFormatter value =
            if settings.use24Hours then
                paddedFormatter value
            else if value == 0 then
                "12"
            else if value > 12 then
                toString (value - 12)
            else
                toString value

        selectionOption : String -> Bool -> Msg -> Html Msg
        selectionOption valueText isSelected msg =
            li [ onClick msg, classList [ ( "elm-time-picker-panel-select-option-selected", isSelected ) ] ]
                [ text valueText ]

        toOption : (a -> String) -> (Time -> a) -> (a -> Msg) -> a -> Html Msg
        toOption formatter accessor toMsg value =
            let
                isSelected =
                    Maybe.map accessor model.value == Just value
            in
                selectionOption (formatter value) isSelected (toMsg value)

        onPicker ev =
            Json.Decode.succeed
                >> onWithOptions ev
                    { preventDefault = True
                    , stopPropagation = True
                    }

        hourOptions =
            if settings.showHours then
                [ div [ class (cssPrefix ++ "panel-select") ]
                    [ ul [] (List.map (toOption hourFormatter .hours SelectHour) hours) ]
                ]
            else
                []

        minuteOptions =
            if settings.showMinutes then
                [ div [ class (cssPrefix ++ "panel-select") ]
                    [ ul [] (List.map (toOption paddedFormatter .minutes SelectMinute) minutes) ]
                ]
            else
                []

        secondOptions =
            if settings.showSeconds then
                [ div [ class (cssPrefix ++ "panel-select") ]
                    [ ul [] (List.map (toOption paddedFormatter .seconds SelectSecond) seconds) ]
                ]
            else
                []

        isInAM =
            model.value
                |> Maybe.map (.hours >> (\hours -> hours < 12))
                |> Maybe.withDefault True

        showPeriodOptions =
            if not settings.use24Hours && settings.showHours then
                [ div [ class (cssPrefix ++ "panel-select") ]
                    [ ul []
                        [ selectionOption "AM" isInAM (SelectPeriod AM)
                        , selectionOption "PM" (not isInAM) (SelectPeriod PM)
                        ]
                    ]
                ]
            else
                []

        optionsDisplay =
            if model.open && not settings.disabled then
                [ div [ class (cssPrefix ++ "panel-combobox"), onPicker "mousedown" NoOp, onPicker "mouseup" NoOp ] <|
                    hourOptions
                        ++ minuteOptions
                        ++ secondOptions
                        ++ showPeriodOptions
                ]
            else
                []

        hoursDisplay time =
            if settings.showHours then
                [ hourFormatter time.hours ]
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

        periodDisplay time =
            if settings.showHours && not settings.use24Hours then
                if isInAM then
                    " AM"
                else
                    " PM"
            else
                ""

        timePartsDisplay time =
            [ hoursDisplay time, minutesDisplay time, secondsDisplay time ]
                |> List.concat
                |> String.join ":"

        formatValueDisplay time =
            timePartsDisplay time ++ periodDisplay time

        inputValue =
            model.value
                |> Maybe.map (formatValueDisplay >> Html.Attributes.value >> List.singleton)
                |> Maybe.withDefault []

        clearButton =
            model.value
                |> Maybe.map (\_ -> [ a [ class (cssPrefix ++ "panel-clear-btn"), href "javascript:void(0);", onPicker "mousedown" NoOp, onPicker "mouseup" NoOp, onClick Clear ] [] ])
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
