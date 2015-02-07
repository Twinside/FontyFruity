-- | This module provide internal access to many structure,
-- not exported by default. The stability of this module is
-- absolutely not insured.
module Graphics.Text.TrueType.Internal
    ( Font( .. )
    , FontHeader( .. )
    , HeaderFlags( .. )
    , HorizontalHeader( .. )
    , HorizontalMetric( .. )
    , HorizontalMetricsTable( .. )
    , MaxpTable( .. )
    , FWord( .. )
    , Fixed( .. )
    ) where

import Graphics.Text.TrueType.MaxpTable
import Graphics.Text.TrueType.HorizontalInfo
import Graphics.Text.TrueType.FontType
import Graphics.Text.TrueType.Header
import Graphics.Text.TrueType.Types

