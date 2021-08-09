module Main 
  ( main
  ) where

import qualified Reservation.UseCaseSpec as UseCase

import           Test.Tasty

main :: IO ()
main = defaultMain root

root :: TestTree
root = testGroup "Reservation API" 
  [ UseCase.spec
  ]
