module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Pipes (yield)
import Pipes.Core ((//>), runEffect, Producer_)

type Result = Int

type MyMonad m r = Producer_ String m r

myLog
  :: forall m
   . Monad m
  => String
  -> MyMonad m Unit
myLog = yield

foo'
  :: forall m
   . Monad m
  => Int
  -> Int
  -> MyMonad m Result
foo' x y = do
  myLog $ "About to compute: " <> show x <> " + " <> show y
  r <- pure $ x + y
  myLog $ "Computed: " <> show x <> " + " <> show y <> " = " <> show r
  pure r

main :: Eff (console :: CONSOLE) Unit
main = do
  let
    go = do
      foo' 1 2

  r <- runEffect $ go //> (liftEff <<< log <<< append "Log >>> ")
  log $ "Result >>> " <> show r
