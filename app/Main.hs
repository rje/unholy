module Main where

import Unholy.Application (runApplication)
import PulseCourier (pulseCourier)
import CircuitBreaker (circuitBreaker)

main :: IO ()
main=runApplication [pulseCourier,circuitBreaker]
