module TimePickerTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TimePicker exposing (..)



-- type Msg
--     = DefaultTimePickerMsg TimePicker.Msg
--     | SteppingTimePickerMsg TimePicker.Msg
--     | PartiallyDisabledTimePickerMsg TimePicker.Msg
-- testSettings : Settings
-- testSettings =
--     { showHours = True
--     , showMinutes = True
--     , showSeconds = True
--     , use24Hours = False
--     , placeholder = "Select time"
--     , hourStep = 1
--     , minuteStep = 1
--     , secondStep = 1
--     , disabled = False
--     , hideDisabledOptions = False
--     , isHourDisabled = always False
--     , isMinuteDisabled = always False
--     , isSecondDisabled = always False
--     , isPeriodDisabled = always False
--     }
-- testTimePicker : TimePicker
-- testTimePicker =
--     { open = False
--     , value = 1
--     , inputText = Nothing
--     }
{- Need to test update and view -}


suite : Test
suite =
    describe "The TimePicker Module"
        [ describe "divisibleByStep function"
            [ test "something" <| \_ -> Expect.equal True (divisibleByStep 15 15) ]
        ]
