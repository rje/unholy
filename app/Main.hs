module Main where

import Unholy.Application (runApplication)
import PulseCourier (pulseCourier)
import LastCue (lastCue)
import Threshold (threshold)
import CircuitBreaker (circuitBreaker)

main :: IO ()
main=runApplication [pulseCourier,circuitBreaker,threshold,lastCue]
