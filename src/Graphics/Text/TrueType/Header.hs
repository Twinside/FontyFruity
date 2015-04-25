{-# LANGUAGE CPP #-}
module Graphics.Text.TrueType.Header
    ( FontHeader( .. )
    , FontStyle( .. )
    , HeaderFlags( .. )
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<*>), (<$>) )
#endif

import Control.DeepSeq( NFData( .. ) )
import Data.Bits( (.|.), setBit, testBit )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( getWord16be
                      , getWord32be
                      , getWord64be
                      )

import Data.Binary.Put( putWord16be)

import Data.Int( Int16 )
import Data.List( foldl' )
import Data.Word( Word16, Word32, Word64 )

import Graphics.Text.TrueType.Types

-- | Describe the basic stylistic properties
-- of a font.
data FontStyle = FontStyle
  { _fontStyleBold   :: !Bool -- ^ If the font is bold.
  , _fontStyleItalic :: !Bool -- ^ If the font is italic.
  }
  deriving (Eq, Ord, Show)

instance Binary FontStyle where
  put style = putWord16be $ italicByte .|. boldByte
    where
      boldByte
        | _fontStyleBold style = 1
        | otherwise = 0

      italicByte
        | _fontStyleItalic style = 2
        | otherwise = 0

  get = do
    styleWord <- getWord16be 
    let bitAt = testBit styleWord
    return $ FontStyle (bitAt 0) (bitAt 1)

-- | Font header
data FontHeader = FontHeader
    { -- | Table version number	0x00010000 for version 1.0.
      _fHdrVersionNumber    :: !Fixed
      -- | fontRevision	Set by font manufacturer.
    , _fHdrFontRevision     :: !Fixed
      -- | To compute:  set it to 0, sum the entire font as
      -- ULONG, then store 0xB1B0AFBA - sum.
    , _fHdrChecksumAdjust   :: !Word32
     
      -- | Should be equal to 0x5F0F3CF5.
    , _fHdrMagicNumber      :: !Word32
    , _fHdrFlags            :: !HeaderFlags

      -- | Valid range is from 16 to 16384
    , _fUnitsPerEm          :: !Word16
      -- | International date (8-byte field).
    , _fHdrCreateTime       :: !Word64
      -- | International date (8-byte field).
    , _fHdrModificationTime :: !Word64

      -- | For all glyph bounding boxes.
    , _fHdrxMin             :: !FWord
      -- | For all glyph bounding boxes.
    , _fHdrYMin             :: !FWord
      -- | For all glyph bounding boxes.
    , _fHdrXMax             :: !FWord
      -- | For all glyph bounding boxes.
    , _fHdrYMax             :: !FWord
      -- | Bit 0 bold (if set to 1); Bit 1 italic (if set to 1)
    , _fHdrMacStyle         :: !FontStyle
      -- | Smallest readable size in pixels.
    , _fHdrLowestRecPPEM    :: !Word16

      -- | 0   Fully mixed directional glyphs;
      --  1   Only strongly left to right;
      --  2   Like 1 but also contains neutrals ;
      -- -1   Only strongly right to left;
      -- -2   Like -1 but also contains neutrals.
    , _fHdrFontDirectionHint :: !Int16
      -- | 0 for short offsets, 1 for long.
    , _fHdrIndexToLocFormat  :: !Int16
      -- | 0 for current format.
    , _fHdrGlyphDataFormat   :: !Int16
    }
    deriving (Eq, Show)

instance NFData FontHeader where
  rnf (FontHeader {}) = ()

instance Binary FontHeader where
  put _ = fail "Unimplemented"
  get =
    FontHeader <$> get <*> get <*> g32 <*> g32 <*> get
               <*> g16 <*> g64 <*> g64 <*> get <*> get
               <*> get <*> get <*> get <*> g16 <*> gi16
               <*> gi16 <*> gi16
      where g16 = getWord16be
            g32 = getWord32be
            gi16 = fromIntegral <$> getWord16be
            g64 = getWord64be


-- | Header flags.
data HeaderFlags = HeaderFlags
    { -- | Bit 0 - baseline for font at y=0;
      _hfBaselineY0           :: !Bool
      -- | Bit 1 - left sidebearing at x=0;
    , _hfLeftSideBearing      :: !Bool
      -- | Bit 2 - instructions may depend on point size;
    , _hfInstrDependPointSize :: !Bool
      -- | Bit 3 - force ppem to integer values for all internal
      -- scaler math; may use fractional ppem sizes if this bit
      -- is clear;
    , _hfForcePPEM            :: !Bool
      -- | Bit 4 - instructions may alter advance width (the
      -- advance widths might not scale linearly);
    , _hfAlterAdvance         :: !Bool
    }
    deriving (Eq, Show)

instance Binary HeaderFlags where
    get = do
      flags <- getWord16be
      let at ix = flags `testBit` ix
      return $ HeaderFlags (at 0) (at 1) (at 2) (at 3) (at 4)

    put (HeaderFlags a0 a1 a2 a3 a4) =
      putWord16be . foldl' setter 0 $ zip [0..] [a0, a1, a2, a3, a4]
        where setter acc (_, False) = acc
              setter acc (ix, True) = setBit acc ix

