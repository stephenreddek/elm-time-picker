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
 Note: Hours are counted from midnight at 0
-}
type alias Time =
    { hours : Int
    , minutes : Int
    , seconds : Int
    , period : Period
    }


{-| Contains the configuration that doesn't need to be maintained by the library
-}
type alias Settings =
    { showMinutes : Bool
    , showSeconds : Bool
    , formatWith24Hours : Bool
    , placeholder : String
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
    { showMinutes = True
    , showSeconds = True
    , formatWith24Hours = False
    , placeholder = "Select time"
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
    , period = AM
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
                TimePicker { model | value = Just { timeToUpdate | period = period } }

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
        hours =
            List.range 0 11

        minutes =
            List.range 0 59

        seconds =
            List.range 0 59

        paddedFormatter value =
            if value < 10 then
                "0" ++ toString value
            else
                toString value

        hourFormatter value =
            if value == 0 then
                "12"
            else
                toString value

        toOption : (a -> String) -> (Time -> a) -> (a -> Msg) -> a -> Html Msg
        toOption formatter accessor toMsg value =
            let
                isSelected =
                    Maybe.map accessor model.value == Just value
            in
                li [ onClick (toMsg value), classList [ ( "elm-time-picker-panel-select-option-selected", isSelected ) ] ]
                    [ text (formatter value) ]

        onPicker ev =
            Json.Decode.succeed
                >> onWithOptions ev
                    { preventDefault = True
                    , stopPropagation = True
                    }

        optionsDisplay =
            if model.open then
                [ div [ class (cssPrefix ++ "panel-combobox"), onPicker "mousedown" NoOp, onPicker "mouseup" NoOp ]
                    [ div [ class (cssPrefix ++ "panel-select") ]
                        [ ul [] (List.map (toOption hourFormatter .hours SelectHour) hours) ]
                    , div [ class (cssPrefix ++ "panel-select") ]
                        [ ul [] (List.map (toOption paddedFormatter .minutes SelectMinute) minutes) ]
                    , div [ class (cssPrefix ++ "panel-select") ]
                        [ ul [] (List.map (toOption paddedFormatter .seconds SelectSecond) seconds) ]
                    , div [ class (cssPrefix ++ "panel-select") ]
                        [ ul []
                            [ toOption toString .period SelectPeriod AM
                            , toOption toString .period SelectPeriod PM
                            ]
                        ]
                    ]
                ]
            else
                []

        formatValueDisplay time =
            hourFormatter time.hours ++ ":" ++ paddedFormatter time.minutes ++ ":" ++ paddedFormatter time.seconds ++ " " ++ toString time.period

        inputValue =
            model.value
                |> Maybe.map (formatValueDisplay >> Html.Attributes.value >> List.singleton)
                |> Maybe.withDefault []

        clearButton =
            model.value
                |> Maybe.map (\_ -> [ a [ class (cssPrefix ++ "panel-clear-btn"), href "javascript:void(0);", onPicker "mousedown" NoOp, onPicker "mouseup" NoOp, onClick Clear ] [] ])
                |> Maybe.withDefault []
    in
        div [ class (cssPrefix ++ "container") ] <|
            [ div [ class (cssPrefix ++ "input-container") ] <|
                [ input ([ type_ "text", onFocus Focus, onBlur Blur, placeholder settings.placeholder ] ++ inputValue) [] ]
                    ++ clearButton
            ]
                ++ optionsDisplay
