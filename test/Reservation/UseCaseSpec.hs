module Reservation.UseCaseSpec (spec) where

import qualified Reservation.Model as Model
import Reservation.ModelGen
import Reservation.SemUtil
import qualified Reservation.UseCase as UseCase

import Test.Tasty
import Test.Tasty.Hedgehog

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Relude.Extra.Map as Map

spec :: TestTree
spec =
    testGroup
        "Reservation.UseCaseSpec"
        [ testProperty "confirmReservation" (prop_confirmReservation maxSeatsRng)
        , testProperty "cancelReservation" (prop_cancelReservation maxSeatsRng)
        , testProperty "getAvailableSeats" (prop_getAvailableSeats maxSeatsRng)
        ]

maxSeatsRng :: Range Model.Seats
maxSeatsRng = Range.constant 10 100

prop_confirmReservation :: Range Model.Seats -> Property
prop_confirmReservation maxSeats = property $ do
    ms <- forAll $ Gen.integral maxSeats
    rsm <- forAll $ reservationMapGen maxSeats
    let rr = concat (Map.elems rsm)
    r <-
        forAll $
            Gen.frequency
                [ (1, Gen.element rr)
                , (2, Gen.filter (`notElem` rr) (reservationGen maxSeats))
                ]
    case runPure ms rsm (UseCase.confirmReservation r) of
        Left (Model.ReservationAlreadyExists r') ->
            label "Exists already" >> assert (r' `elem` concat (Map.elems rsm))
        Left (Model.SeatsNotAvailable actualSeats) ->
            do
                annotateShow actualSeats
                label "Lack of seats" >> assert (actualSeats < Model.reservationSeats r)
        Right (rsm', _) ->
            label "Successful" >> assert (r `elem` concat (Map.elems rsm'))
        _ -> failure

prop_cancelReservation :: Range Model.Seats -> Property
prop_cancelReservation maxSeats = property $ do
    ms <- forAll $ Gen.integral maxSeats
    rsm <- forAll $ reservationMapGen maxSeats
    let rr = concat (Map.elems rsm)
    r <-
        forAll $
            Gen.frequency
                [ (1, Gen.element rr)
                , (1, Gen.filter (`notElem` rr) (reservationGen maxSeats))
                ]
    case runPure ms rsm (UseCase.cancelReservation r) of
        Left (Model.ReservationNotFound r') ->
            do
                annotateShow r'
                label "Not found" >> assert (r' `notElem` concat (Map.elems rsm))
        Right (rsm', _) ->
            label "Successful" >> assert (r `notElem` concat (Map.elems rsm'))
        Left e -> annotateShow e >> failure

prop_getAvailableSeats :: Range Model.Seats -> Property
prop_getAvailableSeats maxSeats = property $ do
    ms <- forAll $ Gen.integral maxSeats
    rsm <- forAll $ reservationMapGen maxSeats
    k <-
        forAll $
            Gen.frequency
                [ (1, Gen.element (Map.keys rsm))
                , (1, Gen.filter (`Map.notMember` rsm) dayGen)
                ]
    case runPure ms rsm (UseCase.getAvailableSeats k) of
        Right (_, actual) -> do
            annotateShow actual
            let expected = Model.seatsAvailable ms (fromMaybe [] (Map.lookup k rsm))
            annotateShow expected
            assert (actual == expected)
        Left e -> annotateShow e >> failure
