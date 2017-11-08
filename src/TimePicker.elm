module TimePicker exposing (Time, TimePicker, Settings, Period(..), Msg, TimeEvent(..), defaultSettings, selectedTime, init, update, view)

{-| A time picker in pure elm.

# Models
@docs Time, TimePicker, Settings, Period, defaultSettings, selectedTime

# Update
@docs init, Msg, TimeEvent, update

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


{-| The base way to represent time. Hours are counted in 24-hour format from midnight at 0
-}
type alias Time =
    { hours : Int
    , minutes : Int
    , seconds : Int
    }


{-| Contains the configuration that doesn't need to be maintained by the library.

- isHourDisabled - Will be run on the 24-hour version of the hour.
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
    , inputText : Maybe String
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
    | TextChanged String
    | SubmitText String


{-| Used to communicate to the caller that the value has been set, changed, or cleared.
-}
type TimeEvent
    = NoChange
    | Changed (Maybe Time)


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
        , value = initialValue
        , inputText = Nothing
        }


defaultTime : Time
defaultTime =
    { hours = 0
    , minutes = 0
    , seconds = 0
    }


{-| Function to update the model when messages come
-}
update : Settings -> Msg -> TimePicker -> ( TimePicker, TimeEvent )
update settings msg (TimePicker ({ value } as model)) =
    case msg of
        Clear ->
            ( TimePicker { model | open = False, value = Nothing }, Changed Nothing )

        Focus ->
            ( TimePicker { model | open = True }, NoChange )

        Blur ->
            ( TimePicker { model | open = False }, NoChange )

        SelectHour hours ->
            let
                timeToUpdate =
                    Maybe.withDefault defaultTime value

                updatedTime =
                    Just { timeToUpdate | hours = hours }
            in
                ( TimePicker { model | value = updatedTime, inputText = Nothing }, Changed updatedTime )

        SelectMinute minutes ->
            let
                timeToUpdate =
                    Maybe.withDefault defaultTime value

                updatedTime =
                    Just { timeToUpdate | minutes = minutes }
            in
                ( TimePicker { model | value = updatedTime, inputText = Nothing }, Changed updatedTime )

        SelectSecond seconds ->
            let
                timeToUpdate =
                    Maybe.withDefault defaultTime value

                updatedTime =
                    Just { timeToUpdate | seconds = seconds }
            in
                ( TimePicker { model | value = updatedTime, inputText = Nothing }, Changed updatedTime )

        SelectPeriod period ->
            let
                updatedTime =
                    value
                        |> Maybe.withDefault defaultTime
                        |> setTimeWithPeriod period
                        |> Just
            in
                ( TimePicker { model | value = updatedTime, inputText = Nothing }, Changed updatedTime )

        NoOp ->
            ( TimePicker model, NoChange )

        TextChanged text ->
            ( TimePicker { model | inputText = Just text }, NoChange )

        SubmitText text ->
            let
                parsedTime =
                    parseText settings text

                isValidInput =
                    parsedTime
                        |> Result.map
                            (Maybe.map (isValidTime settings)
                                >> Maybe.withDefault True
                            )
                        |> Result.withDefault False

                updatedValue =
                    if isValidInput then
                        Result.withDefault value parsedTime
                    else
                        value

                timeEvent =
                    if updatedValue == value then
                        NoChange
                    else
                        Changed updatedValue
            in
                ( TimePicker { model | inputText = Nothing, value = updatedValue }, timeEvent )


setTimeWithPeriod : Period -> Time -> Time
setTimeWithPeriod period time =
    case period of
        AM ->
            if time.hours >= 12 then
                { time | hours = time.hours - 12 }
            else
                time

        PM ->
            if time.hours >= 12 then
                time
            else
                { time | hours = time.hours + 12 }


period : Time -> Period
period time =
    if time.hours >= 12 then
        PM
    else
        AM


isValidTime : Settings -> Time -> Bool
isValidTime settings time =
    let
        isValidPeriod =
            settings.use24Hours || not (settings.isPeriodDisabled <| period time)

        isValidHour =
            (not (settings.isHourDisabled time.hours))
                && (time.hours >= 0)
                && (time.hours <= 23)

        isValidMinute =
            (not (settings.isMinuteDisabled time.minutes))
                && (time.minutes >= 0)
                && (time.minutes <= 59)

        isValidSecond =
            (not (settings.isSecondDisabled time.seconds))
                && (time.seconds >= 0)
                && (time.seconds <= 59)
    in
        isValidHour && isValidMinute && isValidSecond && isValidPeriod


parsePeriod : String -> Result () (Maybe Period)
parsePeriod text =
    case String.toLower text of
        "am" ->
            Ok (Just AM)

        "pm" ->
            Ok (Just PM)

        _ ->
            Err ()


parseTimeParts : Settings -> Maybe Period -> List Int -> Result () (Maybe Time)
parseTimeParts settings period timeParts =
    let
        setHours hours time =
            { time | hours = hours }

        setMinutes minutes time =
            { time | minutes = minutes }

        setSeconds seconds time =
            { time | seconds = seconds }

        partSetter val setter =
            if val then
                [ setter ]
            else
                []

        allSetters =
            partSetter settings.showHours setHours
                ++ partSetter settings.showMinutes setMinutes
                ++ partSetter settings.showSeconds setSeconds

        adjustFor12HourInput time =
            if time.hours == 12 then
                case period of
                    Just AM ->
                        { time | hours = 0 }

                    Just PM ->
                        time

                    Nothing ->
                        if settings.use24Hours then
                            time
                        else
                            { time | hours = 0 }
            else
                time

        withPeriod =
            period
                |> Maybe.map setTimeWithPeriod
                |> Maybe.withDefault identity
    in
        if List.isEmpty timeParts then
            Ok Nothing
        else if List.length timeParts > List.length allSetters then
            Err ()
        else
            List.map2 (,) timeParts allSetters
                |> List.foldl (\( val, setter ) -> setter val) defaultTime
                |> adjustFor12HourInput
                |> withPeriod
                |> Just
                |> Ok


parseText : Settings -> String -> Result () (Maybe Time)
parseText settings text =
    let
        combineTimeParts =
            let
                step e acc =
                    case e of
                        Err _ ->
                            Err ()

                        Ok x ->
                            Result.map ((::) x) acc
            in
                List.foldr step (Ok [])

        splitText =
            text
                |> String.trim
                |> String.split " "

        period =
            splitText
                |> List.drop 1
                |> List.head
                |> Maybe.map parsePeriod
                |> Maybe.withDefault (Ok Nothing)
    in
        if List.length splitText > 2 then
            Err ()
        else
            case period of
                Ok parsedPeriod ->
                    if String.isEmpty (String.trim text) then
                        Ok Nothing
                    else
                        splitText
                            |> List.head
                            |> Result.fromMaybe ()
                            |> Result.map (String.split ":")
                            |> Result.andThen (List.map String.toInt >> combineTimeParts)
                            |> Result.andThen (parseTimeParts settings parsedPeriod)

                Err () ->
                    Err ()


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


onChange : (String -> Msg) -> Html.Attribute Msg
onChange handler =
    on "change" (Json.Decode.map handler targetValue)


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

        chosenTimeValue =
            model.value
                |> Maybe.map (formatValue settings)
                |> Maybe.withDefault ""
                |> Html.Attributes.value

        inputValue =
            model.inputText
                |> Maybe.map Html.Attributes.value
                |> Maybe.withDefault chosenTimeValue

        optionalClear =
            if settings.disabled then
                []
            else
                [ onClick Clear ]

        optionalFocusOnClick =
            if not model.open then
                [ onClick Focus ]
            else
                []

        clearButton =
            model.value
                |> Maybe.map (\_ -> [ a ([ class (cssPrefix ++ "panel-clear-btn"), href "javascript:void(0);", onWithoutLosingFocus "mousedown" NoOp, onWithoutLosingFocus "mouseup" NoOp ] ++ optionalClear) [] ])
                |> Maybe.withDefault []
    in
        div [ classList [ ( cssPrefix ++ "container", True ), ( cssPrefix ++ "active", model.open ) ] ]
            [ div [ class (cssPrefix ++ "inner-container") ] <|
                [ div [ class (cssPrefix ++ "input-container") ] <|
                    [ input
                        ([ type_ "text"
                         , onFocus Focus
                         , onBlur Blur
                         , placeholder settings.placeholder
                         , disabled settings.disabled
                         , onInput TextChanged
                         , onChange SubmitText
                         , inputValue
                         ]
                            ++ optionalFocusOnClick
                        )
                        []
                    ]
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
                    if isDisabled || isSelected then
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
