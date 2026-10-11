module Main where

import Unholy.Application (runApplication)
import PulseCourier (pulseCourier)

main :: IO ()
main=runApplication [pulseCourier]
