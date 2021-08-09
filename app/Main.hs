module Main (main) where

import Reservation.Config
import Network.Wai.Handler.Warp (run)
import Database.Redis.Sentinel (defaultConnectInfo)
import Reservation.App

main :: IO ()
main = do
  let cfg = Config defaultConnectInfo 100 8080
  putStrLn ("Starting server at port " <> show (cfgPort cfg) <> "...")
  run (fromIntegral (cfgPort cfg)) (createApp cfg)