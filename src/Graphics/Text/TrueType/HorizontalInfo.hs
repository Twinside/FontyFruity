{-# LANGUAGE CPP #-}
module Graphics.Text.TrueType.HorizontalInfo
    ( HorizontalHeader( .. )
    , HorizontalMetricsTable( .. )
    , HorizontalMetric( .. )
    , getHorizontalMetrics
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<*>) )
#endif

import Control.Applicative( (<$>) )

import Control.DeepSeq( NFData( .. ) )
import Control.Monad( when, replicateM_ )
import Data.Word( Word16 )
import Data.Int( Int16 )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( Get, getWord16be, getWord32be )
import Data.Binary.Put( putWord16be, putWord32be )
import qualified Data.Vector as V

import Graphics.Text.TrueType.Types

-- | Describe the "hhea" TrueType table.
data HorizontalHeader = HorizontalHeader
    { -- | Distance from baseline of highest ascender
      _hheaAscent  :: {-# UNPACK #-} !FWord
      -- | Distance from baseline of lowest descender
    , _hheaDescent :: {-# UNPACK #-} !FWord
      -- | typographic line gap
    , _hheaLineGap :: {-# UNPACK #-} !FWord
      -- | must be consistent with horizontal metrics
    , _hheaAdvanceWidthMax :: {-# UNPACK #-} !FWord
      -- | must be consistent with horizontal metrics
    , _hheaMinLeftSideBearing :: {-# UNPACK #-} !FWord
      -- | must be consistent with horizontal metrics
    , _hheaMinRightSideBearing :: {-# UNPACK #-} !FWord
      -- | max(lsb + (xMax-xMin))
    , _hheaXmaxExtent :: {-# UNPACK #-} !FWord
      -- | used to calculate the slope of the caret 
      -- (rise/run) set to 1 for vertical caret
    , _hheaCaretSlopeRise :: {-# UNPACK #-} !Int16
      -- | 0 for vertical
    , _hheaCaretSlopeRun :: {-# UNPACK #-} !Int16
      -- | set value to 0 for non-slanted fonts
    , _hheaCaretOffset  :: {-# UNPACK #-} !FWord
      -- | 0 for current format
    , _hheaMetricDataFormat :: {-# UNPACK #-} !Int16
      -- | number of advance widths in metrics table
    , _hheaLongHorMetricCount :: {-# UNPACK #-} !Word16
    }
    deriving (Eq, Show)

instance NFData HorizontalHeader where
    rnf (HorizontalHeader {}) = ()

instance Binary HorizontalHeader where
  put hdr = do
    putWord32be 0x00010000
    put $ _hheaAscent hdr
    put $ _hheaDescent hdr
    put $ _hheaLineGap hdr
    put $ _hheaAdvanceWidthMax hdr
    put $ _hheaMinLeftSideBearing hdr
    put $ _hheaMinRightSideBearing hdr
    put $ _hheaXmaxExtent hdr
    putWord16be . fromIntegral $ _hheaCaretSlopeRise hdr
    putWord16be . fromIntegral $ _hheaCaretSlopeRun hdr
    put $ _hheaCaretOffset hdr
    replicateM_ 4 $ putWord16be 0
    putWord16be . fromIntegral $ _hheaMetricDataFormat hdr
    putWord16be $ _hheaLongHorMetricCount hdr

  get = do
    ver <- getWord32be
    when (ver /= 0x00010000)
         (fail "Invalid HorizontalHeader (hhea) version")
    startHdr <- HorizontalHeader
            <$> get <*> get <*> get <*> get
            <*> get <*> get <*> get
            <*> (fromIntegral <$> getWord16be)
            <*> (fromIntegral <$> getWord16be)
            <*> get
    replicateM_ 4 getWord16be -- reserved, don't care
    startHdr <$> (fromIntegral <$> getWord16be)
             <*> getWord16be

-- | Information of horizontal advance.
data HorizontalMetric = HorizontalMetric
    { _hmtxAdvanceWidth    :: {-# UNPACK #-} !Word16
    , _hmtxLeftSideBearing :: {-# UNPACK #-} !Int16
    }
    deriving (Eq, Show)

instance Binary HorizontalMetric where
    put (HorizontalMetric adv bear) =
      putWord16be adv >> putWord16be (fromIntegral bear)

    get = HorizontalMetric <$> g16 <*> (fromIntegral <$> g16)
      where g16 = getWord16be

-- | For every glyph (indexed by the glyph index),
-- provide horizontal information.
data HorizontalMetricsTable = HorizontalMetricsTable
    { _glyphMetrics :: !(V.Vector HorizontalMetric)
    }
    deriving (Eq, Show)

getHorizontalMetrics :: Int -- ^ Metrics count
                     -> Int -- ^ Glyph count
                     -> Get HorizontalMetricsTable
getHorizontalMetrics numberOfMetrics glyphCount = do
    hMetrics <- V.replicateM numberOfMetrics get
    let lastAdvance = _hmtxAdvanceWidth $ V.last hMetrics
    run <- V.replicateM sideBearingCount $
        HorizontalMetric lastAdvance . fromIntegral <$> getWord16be
    return $ HorizontalMetricsTable $ V.concat [hMetrics, run]
  where
    sideBearingCount = glyphCount - numberOfMetrics 

