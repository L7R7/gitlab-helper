{-# LANGUAGE OverloadedStrings #-}

module Viz where

import qualified Data.DateTime as DT
import Graphics.Vega.VegaLite
import Pipelines
import Relude

test :: [PipelineWithDuration]
test =
  []

-- PipelineWithDuration (PipelineId 1) (Sha "sha-123") (Duration 123) (Duration 0) (DT.fromSeconds 1000) $$(staticURI "https://my.gitlab.com/projects/512/foo"),
-- PipelineWithDuration (PipelineId 2) (Sha "sha-123") (Duration 231) (Duration 1) (DT.fromSeconds 86402) $$(staticURI "https://my.gitlab.com/projects/512/foo")

plotTimeline :: [PipelineWithDuration] -> VegaLite
plotTimeline entries =
  let dates = map (toText . DT.formatDateTime "%Y-%m-%d-%H-%M" . createdAt) entries
      dat =
        dataFromColumns [Parse [("dates", FoUtc "%Y-%m-%d-%H-%M")]]
          . dataColumn "dates" (Strings dates) -- TODO: investigate hvega DateTime datatype
          . dataColumn "dur" (Strings (map (toText . duration) entries)) -- TODO: investigate hvega DateTime datatype
      enc =
        encoding
          . position X [PName "dates", PmType Temporal, PTimeUnit YearMonthDateHoursMinutesSeconds, PAxis [AxTitle "Date (Days)"]]
          . position Y [PName "dur", PmType Quantitative, PAxis [AxTitle "Pipeline duration"]]
          . row [FName "dates", FmType Temporal, FTimeUnit (Utc YearQuarter)]
   in toVegaLite
        [ dat [],
          mark Point [MFilled True],
          enc [],
          width 800,
          height 200
        ]
