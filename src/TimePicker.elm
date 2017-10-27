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
    }


type alias Model =
    { open : Bool
    , value : Maybe Time
    }


{-| The internal messages that the picker uses to operate
-}
type Msg
    = NoOp


{-| The basic configuration for a TimePicker
-}
defaultSettings : Settings
defaultSettings =
    { showMinutes = True
    , showSeconds = True
    , formatWith24Hours = False
    }


{-| Function for initializing a closed and empty TimePicker
-}
init : Maybe Time -> TimePicker
init initialValue =
    TimePicker (Model False initialValue)


{-| Function to update the model when messages come
-}
update : Settings -> Msg -> TimePicker -> TimePicker
update settings msg (TimePicker model) =
    TimePicker model


{-| Function for viewing the time picker
-}
view : Settings -> TimePicker -> Html Msg
view settings (TimePicker model) =
    div [] []
