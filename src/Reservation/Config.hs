module Reservation.Config (
    Config (..),
) where

import Database.Redis (ConnectInfo)
import Reservation.Model (Seats)

data Config = Config
    { cfgRedis :: !ConnectInfo
    , cfgMaxSeats :: !Seats
    , cfgPort :: !Int
    }
    deriving stock (Show, Generic)
