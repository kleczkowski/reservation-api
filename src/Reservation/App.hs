module Reservation.App (createApp) where

import Reservation.API
import Reservation.Config
import qualified Reservation.KVS as KVS
import qualified Reservation.Model as Model
import qualified Reservation.Redis as Redis
import qualified Reservation.UseCase as UseCase

import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Polysemy.Input (Input)
import qualified Polysemy.Input as Input
import Polysemy.Log (Log)
import qualified Polysemy.Log as Log

import Data.Aeson
import Database.Redis
import Servant

createApp :: Config -> Application
createApp cfg = serve reservationAPI (liftServer cfg)

liftServer :: Config -> ServerT ReservationAPI Handler
liftServer cfg = hoistServer reservationAPI interpretServer reservationServer
  where
    interpretServer
      :: Sem '[ UseCase.ReservationStor
              , Error Model.ReservationError
              , Log
              , Embed Redis
              , Input Connection
              , Input Model.Seats
              , Embed IO
              ] a
      -> Handler a
    interpretServer s = s
      & KVS.runKvsAsRedis
      & Error.runError @Model.ReservationError
      & Log.interpretLogStderr'
      & Redis.runRedis
      & Redis.obtainRedisConnection (cfgRedis cfg)
      & Input.runInputConst (cfgMaxSeats cfg)
      & runM
      & liftToHandler

liftToHandler :: ToJSON e => IO (Either e a) -> Handler a
liftToHandler = Handler . ExceptT . fmap handleErr
  where handleErr (Left e)  = Left (err400 { errBody = encode (toJSON e)})
        handleErr (Right a) = Right a
