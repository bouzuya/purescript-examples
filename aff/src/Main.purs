module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import LiftEff as LiftEff
import Console as Console
import Error as Error

aff :: Aff Unit
aff = do
  LiftEff.aff
  Console.aff
  Error.aff

main :: Effect Unit
main = void $ launchAff aff
