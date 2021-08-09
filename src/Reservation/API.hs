module Reservation.API 
  ( ReservationAPI
  , reservationAPI
  , reservationServer
  ) where

import qualified Reservation.Model as Model
import qualified Reservation.UseCase as UseCase

import Polysemy
import Polysemy.Error (Error)
import Servant
import Data.Time
import Polysemy.Log (Log)
import Polysemy.Input (Input)

-- | Reservation API routing.
type ReservationAPI
  =     "reservation" :> Summary  "Creates a new reservation"
                      :> ReqBody  '[JSON] Model.Reservation
                      :> Post     '[JSON] ()
  :<|>  "reservation" :> Summary  "Removes reservation"
                      :> ReqBody  '[JSON] Model.Reservation
                      :> Delete   '[JSON] ()
  :<|>  "reservation" :> Summary  "Get number of available seats given day"
                      :> Capture' '[Description "Day"] "day" Day
                      :> Get      '[JSON] Model.Seats 

-- | Returns Servant server.
reservationServer 
  :: Members '[ UseCase.ReservationStor
              , Error Model.ReservationError
              , Input Model.Seats
              , Log
              ] r 
  => ServerT ReservationAPI (Sem r)
reservationServer
  =     UseCase.confirmReservation
  :<|>  UseCase.cancelReservation
  :<|>  UseCase.getAvailableSeats

-- | Proxy for @ReservationAPI@.
reservationAPI :: Proxy ReservationAPI
reservationAPI = Proxy
{-# INLINE reservationAPI #-}