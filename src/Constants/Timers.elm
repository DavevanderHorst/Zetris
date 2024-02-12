module Constants.Timers exposing (..)


gameClockWaitTime : Float
gameClockWaitTime =
    50


brickDropTime : Float
brickDropTime =
    -- 1 second
    1000


newBrickWaitTime : Float
newBrickWaitTime =
    1500


rowBlinkTime : Float
rowBlinkTime =
    180


zetrisBlinkTime : Float
zetrisBlinkTime =
    90


numberOfRowBlinks : Int
numberOfRowBlinks =
    -- 6x rowBlinkTime
    10


zetrisRowBlinks : Int
zetrisRowBlinks =
    25


startBrickDropTime : Float
startBrickDropTime =
    -- 2 second
    2000


activatePlayerInPutWaitTime : Float
activatePlayerInPutWaitTime =
    100
