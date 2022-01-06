module Graphics.Text.TrueType.FontType
    ( Font( .. )
    , emptyFont
    ) where

import Control.DeepSeq( NFData( .. ) )
import Data.Word( Word32 )
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Graphics.Text.TrueType.MaxpTable
import Graphics.Text.TrueType.Glyph
import Graphics.Text.TrueType.Header
import Graphics.Text.TrueType.OffsetTable
import Graphics.Text.TrueType.CharacterMap
import Graphics.Text.TrueType.Kerning
import Graphics.Text.TrueType.HorizontalInfo
import Graphics.Text.TrueType.Name


-- | Type representing a font.
data Font = Font
 { -- | Field discribing various offsets/positions of table
   -- inside the font file. Not available for reading.
   _fontOffsetTable       :: !OffsetTable
 , _fontTables            :: ![(B.ByteString, B.ByteString)]
 , _fontNames             :: Maybe NameTable
 , _fontHeader            :: Maybe FontHeader
 , _fontMaxp              :: Maybe MaxpTable
 , _fontMap               :: Maybe CharacterMaps
 , _fontKerning           :: Maybe KernTable
 , _fontGlyph             :: Maybe (V.Vector Glyph)
 , _fontLoca              :: Maybe (VU.Vector Word32)
 , _fontHorizontalHeader  :: Maybe HorizontalHeader
 , _fontHorizontalMetrics :: Maybe HorizontalMetricsTable
 }
 deriving (Show)

instance NFData Font where
    rnf font =
        _fontOffsetTable font `seq`
        _fontTables font `seq`
        _fontGlyph font `seq`
        _fontLoca font `seq`
        ()

emptyFont :: OffsetTable -> Font
emptyFont table = Font
    { _fontTables            = []
    , _fontOffsetTable       = table
    , _fontNames             = Nothing
    , _fontHeader            = Nothing
    , _fontGlyph             = Nothing
    , _fontMaxp              = Nothing
    , _fontLoca              = Nothing
    , _fontMap               = Nothing
    , _fontKerning           = Nothing
    , _fontHorizontalHeader  = Nothing
    , _fontHorizontalMetrics = Nothing
    }

