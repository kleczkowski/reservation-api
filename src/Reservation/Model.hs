{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Reservation.Model (
    Reservation (..),
    ReservationError (..),
    Seats,
    addReservation,
    removeReservation,
    seatsAvailable,
) where

import Data.Aeson
import Data.Binary
import qualified Data.List as List
import Data.Time

-- | A reservation.
data Reservation = Reservation
    { reservationName :: !Text
    -- ^ The name of person that created reservation.
    , reservationMail :: !Text
    -- ^ The e-mail address of reservation issuer.
    , reservationDay :: !Day
    -- ^ The date when reservation will happen.
    , reservationSeats :: !Seats
    -- ^ The number of seats to be taken.
    }
    deriving stock (Eq, Ord, Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON)

deriving newtype instance Binary Day
deriving anyclass instance Binary Reservation

-- | An error during reservation.
data ReservationError
    = -- | Reservation already exists in store.
      ReservationAlreadyExists !Reservation
    | -- | There is no such reservation in store.
      ReservationNotFound !Reservation
    | -- | Cannot add reservation because there is no seat available.
      --   Provides current number of reserved seats.
      SeatsNotAvailable !Seats
    deriving stock (Eq, Ord, Show, Read, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Type alias for the number of seats.
type Seats = Natural

-- | Computes the sum of occupied seats at given day.
seatsOccupied :: [Reservation] -> Seats
seatsOccupied = sum . map reservationSeats
{-# INLINE seatsOccupied #-}

-- | Computes available number of seats to be reserved.
seatsAvailable :: Seats -> [Reservation] -> Seats
seatsAvailable maxSeats rs
    | maxSeats >= seatsOccupied rs = maxSeats - seatsOccupied rs
    | otherwise = 0
{-# INLINE seatsAvailable #-}

{- | Adds reservation to the list if there are enough seats
  and reservation is not duplicated,
  otherwise returns @Nothing@.
-}
addReservation ::
    Seats ->
    Reservation ->
    [Reservation] ->
    Either ReservationError [Reservation]
addReservation maxSeats r rs
    | r `List.elem` rs = Left (ReservationAlreadyExists r)
    | seatsNow >= reservationSeats r = Right (r : rs)
    | otherwise = Left (SeatsNotAvailable seatsNow)
  where
    seatsNow = seatsAvailable maxSeats rs

-- | Removes existing reservation from list.
removeReservation ::
    Reservation ->
    [Reservation] ->
    Either ReservationError [Reservation]
removeReservation r rs
    | r `List.elem` rs = Right (List.delete r rs)
    | otherwise = Left (ReservationNotFound r)
