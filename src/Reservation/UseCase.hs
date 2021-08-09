module Reservation.UseCase
  ( confirmReservation
  , cancelReservation
  , getAvailableSeats
  , ReservationStor
  ) where

import Reservation.KVS (KVS)
import qualified Reservation.KVS as KVS
import qualified Reservation.Model as Model

import Data.Time
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Polysemy.Log (Log)
import qualified Polysemy.Log as Log

-- | Type alias for key-value reservation store.
type ReservationStor = KVS Day [Model.Reservation]

-- | Creates new reservation in the key-value store.
confirmReservation
  :: Members '[ ReservationStor
              , Error Model.ReservationError
              , Input Model.Seats
              , Log
              ] r
  => Model.Reservation
  -> Sem r ()
confirmReservation r = do
  let k = Model.reservationDay r
  maxSeats <- Input.input
  rs <- fromMaybe [] <$> KVS.kvsLookup k
  flip (either Error.throw) (Model.addReservation maxSeats r rs) $ \rs' -> do
    KVS.kvsInsert k rs'
    Log.info ("Added reservation: " <> show r)

-- | Removes existing reservation in the key-value store.
cancelReservation
  :: Members '[ ReservationStor
              , Error Model.ReservationError
              , Log
              ] r
  => Model.Reservation
  -> Sem r ()
cancelReservation r = do
  let k = Model.reservationDay r
  rs <- fromMaybe [] <$> KVS.kvsLookup k
  flip (either Error.throw) (Model.removeReservation r rs) $ \rs' -> do
    KVS.kvsInsert k rs'
    Log.info ("Removed reservation: " <> show r)

-- | Gets the number of available seats given day.
getAvailableSeats
  :: Members '[ ReservationStor
              , Input Model.Seats
              , Log
              ] r
  => Day
  -> Sem r Model.Seats
getAvailableSeats k = do
  maxSeats <- Input.input
  rs <- fromMaybe [] <$> KVS.kvsLookup k
  let n = Model.seatsAvailable maxSeats rs
  Log.info ("Available seats: " <> show n)
  pure n
