module Reservation.SemUtil (runPure) where

import Prelude hiding (State)

import qualified Reservation.Model as Model
import qualified Reservation.UseCase as UseCase
import qualified Reservation.KVS as KVS

import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Polysemy.Log (Log)
import qualified Polysemy.Log as Log
import qualified Polysemy.State as State

import Data.Time

runPure 
  :: Model.Seats 
  -> Map Day [Model.Reservation]
  -> Sem '[ UseCase.ReservationStor
          , Error Model.ReservationError
          , Input Model.Seats
          , Log
          ] a
  -> Either Model.ReservationError (Map Day [Model.Reservation], a)
runPure maxSeats m s = s
  & KVS.runKvsAsMap
  & State.runState m
  & Error.runError @Model.ReservationError
  & Input.runInputConst maxSeats
  & Log.interpretLogNull
  & run