module Test where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Sound.Tidal.Pattern

main :: IO ()
main = hspec $ do
  describe "Sound.Tidal.Pattern.eventWhole" $ do
    it "returns the second element of a tuple inside the first element of a tuple" $ do
      property $ (3,4) == eventWhole (((1,2),(3,4)), 5)

  describe "Sound.Tidal.Pattern.eventPart" $ do
    it "returns the first element of a tuple inside the first element of a tuple" $ do
      property $ (1,2) == eventPart (((1,2),(3,4)), 5)

  describe "Sound.Tidal.Pattern.delta" $ do
    it "subtracts the second element of a tuple from the first" $ do
      property $ delta (3,10) == 7

  describe "Sound.Tidal.Pattern.atom" $ do
    it "fills a whole cycle" $ do
      property $ query (atom 0) (0,1) == [(((0,1),(0,1)),0)]
    it "returns the part of an atom that you ask for, preserving the whole" $ do
      property $ query (atom 0) (0.25,0.75) == [(((0.25,0.75),(0,1)),0)]
    it "gives correct fragments when you go over cycle boundaries" $ do
      property $ query (atom 0) (0.25,1.25) == [(((0.25,1),(0,1)),0),
                                                (((1,1.25),(1,2)),0)
                                               ]
