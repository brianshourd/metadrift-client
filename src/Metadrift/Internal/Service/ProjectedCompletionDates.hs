{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, TemplateHaskell, RankNTypes #-}

module Metadrift.Internal.Service.ProjectedCompletionDates where

import qualified Data.Aeson.TH as Aeson
import qualified Data.Text as T
import qualified Data.Time.Calendar as Calendar
import qualified Data.Time.Clock as Clock
import           GHC.Generics (Generic)
import qualified Text.Printf as P
import qualified Metadrift.Internal.Utils as Utils

data ProjectedDates =
       ProjectedDates
         { p5 :: Double
         , p95 :: Double
         , startDate :: Clock.UTCTime
         , breakoutDay :: Clock.UTCTime
         , p5Date :: Clock.UTCTime
         , p50Date :: Clock.UTCTime
         , p95Date :: Clock.UTCTime
         }
  deriving (Generic)

$(Aeson.deriveJSON Utils.defaultAesonOptions ''ProjectedDates)

data T =
       T
         { name :: T.Text
         , title :: T.Text
         , doer :: Maybe T.Text
         , projectedDates :: Maybe ProjectedDates
         }
  deriving (Generic)

$(Aeson.deriveJSON Utils.defaultAesonOptions ''T)

importantDates :: ProjectedDates -> [(String, Clock.UTCTime)]
importantDates p =
  [ ("Breakout", breakoutDay p)
  , ("P50", p50Date p)
  ]

prettyPrint :: Clock.UTCTime -> T -> String
prettyPrint today t = P.printf "Title: %s (%s)\nDoer: %s\nProjected Dates:\n%s" (title t) (name t) printDoer (prettyPrintDates today (projectedDates t))
  where
    printDoer = case doer t of
      Nothing -> "Unset"
      Just x  -> T.unpack x

prettyPrintDates :: Clock.UTCTime -> Maybe ProjectedDates -> String
prettyPrintDates _     Nothing  = "  No estimates\n"
prettyPrintDates today (Just p) = unlines . map (\(dateName, date) -> P.printf "  %s: %s" dateName (printRelativeDate today date)) $ importantDates p

printRelativeDate :: Clock.UTCTime -> Clock.UTCTime -> String
printRelativeDate today toPrint =
  let today' = Clock.utctDay today
      toPrint' = Clock.utctDay toPrint
      diff = abs (Calendar.diffDays toPrint' today') in
  case compare toPrint' today' of
    LT -> P.printf "%d days ago" diff
    GT -> P.printf "%d days from now" diff
    EQ -> "Today"

priority :: Clock.UTCTime -> T -> Int
priority today c = case projectedDates c of
    Nothing -> -1
    Just dates -> minimum
      . map (\(_, date) -> fromIntegral . abs $ Calendar.diffDays (Clock.utctDay today) (Clock.utctDay date))
      $ importantDates dates
