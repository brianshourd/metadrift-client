{-# LANGUAGE OverloadedStrings #-}
module Metadrift.Internal.Service.ProjectedCompletionDatesSpec where

import qualified Data.Time as Time
import qualified Data.Time.Clock as Clock
import           Test.Hspec
import           Metadrift.Internal.Service.ProjectedCompletionDates

main :: IO ()
main = hspec spec

dateTimeFromGregorian :: Int -> Int -> Int -> Clock.UTCTime
dateTimeFromGregorian year month day = Clock.UTCTime (Time.fromGregorian year month day) (Clock.secondsToDiffTime 0)

spec :: Spec
spec =
  describe "prettyPrint" $ do
    it "performs as expected" $ do
      let input = T
            { name = "L7MK12"
            , title = "UI for listing and \"approving\" or \"declining\" of open RequestApprovals."
            , doer = Just "ZGG"
            , projectedDates = Just ProjectedDates
              { p5 = 3
              , p95 = 12
              , startDate = dateTimeFromGregorian 2017 3 10
              , breakoutDay = dateTimeFromGregorian 2017 3 17
              , p5Date = dateTimeFromGregorian 2017 3 15
              , p50Date = dateTimeFromGregorian 2017 3 18
              , p95Date = dateTimeFromGregorian 2017 3 26
              }
            }
      let date = dateTimeFromGregorian 2017 3 24
      let expectedOutput = "Title: UI for listing and \"approving\" or \"declining\" of open RequestApprovals. (L7MK12)\nDoer: ZGG\nProjected Dates:\n  Breakout: 7 days ago\n  P50: 6 days ago\n"
      prettyPrint date input `shouldBe` expectedOutput
