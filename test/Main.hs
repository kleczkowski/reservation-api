module Main 
  ( main
  ) where

import           Test.Tasty

main :: IO ()
main = defaultMain root

root :: TestTree
root = testGroup "Reservation API" 
  [ 
  ]
