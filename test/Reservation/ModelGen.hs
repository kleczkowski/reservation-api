module Reservation.ModelGen (
    reservationMapGen,
    reservationGen,
    dayGen,
) where

import Reservation.Model

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Time
import qualified Relude.Extra.Map as Map

-- | Generates a reservation map.
reservationMapGen :: Range Seats -> Gen (Map Day [Reservation])
reservationMapGen seatsRange = do
    rs <- Gen.list (Range.linear 1 500) (reservationGen seatsRange)
    pure (foldr addToMap (fromList []) rs)
  where
    addToMap r = Map.alter (Just . maybe [r] (r :)) (reservationDay r)

-- | Generates a reservation.
reservationGen :: Range Seats -> Gen Reservation
reservationGen seatsRange =
    Reservation
        <$> Gen.text (Range.constant 1 50) Gen.alphaNum
        <*> Gen.text (Range.constant 1 50) Gen.alphaNum
        <*> dayGen
        <*> Gen.integral seatsRange

dayGen :: Gen Day
dayGen =
    fromGregorian
        <$> Gen.integral (Range.linear 2000 2050)
        <*> Gen.int (Range.linear 1 12)
        <*> Gen.int (Range.constant 1 28)
