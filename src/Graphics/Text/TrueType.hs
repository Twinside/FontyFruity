{-# LANGUAGE TupleSections #-}
module Graphics.Text.TrueType
     where

import Control.Applicative( (<$>), (<*>) )
import Data.Bits( setBit, testBit )
import Data.Function( on )
import Data.Int( Int16 )
import Data.List( sortBy, foldl' )
import Data.Word( Word8, Word16, Word32, Word64 )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( Get
                      , bytesRead
                      , getWord8
                      , getWord16be
                      , getWord32be
                      , getWord64be
                      , getByteString
                      , skip
                      )

import Data.Binary.Put( putWord8
                      , putWord16be
                      , putWord32be
                      , putByteString
                      )

import Data.Monoid( mempty )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

data Fixed = Fixed Word16 Word16
    deriving (Eq, Show)

instance Binary Fixed where
    get = Fixed <$> getWord16be <*> getWord16be
    put (Fixed a b) = putWord16be a >> putWord16be b

newtype FWord = FWord Word16
    deriving (Eq, Show)

instance Binary FWord where
  put (FWord w) = putWord16be w
  get = FWord <$> getWord16be

data OffsetTableHeader = OffsetTableHeader
    { -- | sfnt version 0x00010000 for version 1.0.
      _othSfntVersion   :: !Fixed
      -- | numTables Number of tables.
    , _othTableCount    :: !Word16
      -- | searchRange (Maximum power of 2 ? numTables) x 16.
    , _othSearchRange   :: !Word16
      -- | entrySelector Log2(maximum power of 2 ? numTables).
    , _othEntrySelector :: !Word16
      -- | rangeShift NumTables x 16-searchRange.
    , _othRangeShift    :: !Word16
    }
    deriving (Eq, Show)

instance Binary OffsetTableHeader where
  get = OffsetTableHeader <$> get <*> g16 <*> g16 <*> g16 <*> g16
    where g16 = getWord16be
  put (OffsetTableHeader ver c sr es rs) =
    put ver >> p16 c >> p16 sr >> p16 es >> p16 rs
      where p16 = putWord16be

data TableDirectoryEntry = TableDirectoryEntry
    { -- | tag 4 -byte identifier.
      _tdeTag      :: !BC.ByteString
      -- | CheckSum for this table.
    , _tdeChecksum :: !Word32
      -- | Offset from beginning of TrueType font file.
    , _tdeOffset   :: !Word32
      -- | Length of this table.
    , _tdeLength   :: !Word32
    }
    deriving (Eq, Show)

instance Binary TableDirectoryEntry where
  get = TableDirectoryEntry <$> getByteString 4 <*> g32 <*> g32 <*> g32
    where g32 = getWord32be
  put (TableDirectoryEntry tag chk offset ln) =
      putByteString tag >> p32 chk >> p32 offset >> p32 ln
        where p32 = putWord32be

data OffsetTable = OffsetTable
    { _otHeader  :: !OffsetTableHeader
    , _otEntries :: !(V.Vector TableDirectoryEntry)
    }
    deriving (Eq, Show)

instance Binary OffsetTable where
    put (OffsetTable hdr entries) =
        put hdr >> V.forM_ entries put

    get = do
        hdr <- get
        let count = fromIntegral $ _othTableCount hdr
        OffsetTable hdr <$> V.replicateM count get

data GlyphHeader = GlyphHeader
    { -- | If the number of contours is greater than or equal
      -- to zero, this is a single glyph; if negative, this is
      -- a composite glyph.
      _glfNumberOfContours :: !Int16
      -- | Minimum x for coordinate data.
    , _glfXMin             :: !Int16
      -- | Minimum y for coordinate data.
    , _glfYMin             :: !Int16
      -- | Maximum x for coordinate data.
    , _glfXMax             :: !Int16
      -- | Maximum y for coordinate data.
    , _glfYMax             :: !Int16
    }
    deriving (Eq, Show)

instance Binary GlyphHeader where
    get = GlyphHeader <$> g16 <*> g16 <*> g16 <*> g16 <*> g16
      where g16 = fromIntegral <$> getWord16be

    put (GlyphHeader count xmini ymini xmaxi ymaxi) =
        p16 count >> p16 xmini >> p16 ymini >> p16 xmaxi >> p16 ymaxi
      where p16 = putWord16be . fromIntegral

data GlyphContour = GlyphContour
    { _glyphInstructions :: !(VU.Vector Word8)
    , _glyphPoints       :: !(VU.Vector (Int16, Int16))
    }
    deriving (Eq, Show)

data CompositeScaling
    = CompositeScale Word16
    | CompositeXYScale Word16 Word16
    | Composite2x2 Word16 Word16 Word16 Word16
    | CompositeNoScale
    deriving (Eq, Show)

data GlyphComposition = GlyphComposition
    { _glyphCompositeFlag    :: !Word16
    , _glyphCompositeIndex   :: !Word16
    , _glyphCompositionArg   :: !(Int16, Int16)
    , _glyphCompositionScale :: !CompositeScaling
    }
    deriving (Eq, Show)

data GlyphContent
    = GlyphSimple    GlyphContour
    | GlyphComposite (V.Vector GlyphComposition) (VU.Vector Word8)
    deriving (Eq, Show)

data Glyph = Glyph
    { _glyphHeader  :: !GlyphHeader
    , _glyphContent :: !GlyphContent
    }
    deriving (Eq, Show)

getCompositeOutline :: Get GlyphContent
getCompositeOutline =
    (\(instr, vals) -> GlyphComposite (V.fromList vals) instr) <$> go
  where
    go = do
      flag <- getWord16be
      value <-
          GlyphComposition flag
            <$> getWord16be <*> fetchArguments flag <*> fetchScaling flag

      if flag `testBit` mORE_COMPONENTS then
          (\(instr, acc) -> (instr, value : acc )) <$> go
      else
        if flag `testBit` wE_HAVE_INSTRUCTIONS then do
            count <- fromIntegral <$> getWord16be
            (, [value]) <$> VU.replicateM count getWord8
        else
            return (mempty, [value])


    fetchArguments flag
        | flag `testBit` aRG_1_AND_2_ARE_WORDS =
            (,) <$> getInt16be <*> getInt16be
        | otherwise =
            (,) <$> getInt8 <*> getInt8

    fetchScaling flag
        | flag `testBit` wE_HAVE_A_SCALE = CompositeScale <$> getF2Dot14
        | flag `testBit` wE_HAVE_AN_X_AND_Y_SCALE =
            CompositeXYScale <$> getF2Dot14 <*> getF2Dot14
        | flag `testBit` wE_HAVE_A_TWO_BY_TWO =
            Composite2x2 <$> getF2Dot14 <*> getF2Dot14
                         <*> getF2Dot14 <*> getF2Dot14
        | otherwise = return CompositeNoScale

    getInt16be = fromIntegral <$> getWord16be
    getF2Dot14 = fromIntegral <$> getWord16be
    getInt8 = fromIntegral <$> getWord8

    aRG_1_AND_2_ARE_WORDS  = 0
    {-aRGS_ARE_XY_VALUES  = 1-}
    {-rOUND_XY_TO_GRID  = 2-}
    wE_HAVE_A_SCALE  = 3
    {-rESERVED  = 4-}
    mORE_COMPONENTS  = 5
    wE_HAVE_AN_X_AND_Y_SCALE  = 6
    wE_HAVE_A_TWO_BY_TWO  = 7
    wE_HAVE_INSTRUCTIONS  = 8
    {-uSE_MY_METRICS  = 9-}
    {-oVERLAP_COMPOUND  = 10-}
    {-sCALED_COMPONENT_OFFSET  = 11-}
    {-uNSCALED_COMPONENT_OFFSET = 12-}

data GlyphFlag = GlyphFlag
    { -- | If set, the point is on the curve;
      -- otherwise, it is off the curve.
      _flagOnCurve :: !Bool
      -- | If set, the corresponding x-coordinate is 1
      -- byte long. If not set, 2 bytes.
    , _flagXshort  :: !Bool
      -- | If set, the corresponding y-coordinate is 1
      -- byte long. If not set, 2 bytes.
    , _flagYShort  :: !Bool
      -- | If set, the next byte specifies the number of additional
      -- times this set of flags is to be repeated. In this way, the
      -- number of flags listed can be smaller than the number of
      -- points in a character.
    , _flagRepeat  :: !Bool
      -- | This flag has two meanings, depending on how the x-Short
      -- Vector flag is set. If x-Short Vector is set, this bit
      -- describes the sign of the value, with 1 equalling positive and
      -- 0 negative. If the x-Short Vector bit is not set and this bit is set,
      -- then the current x-coordinate is the same as the previous x-coordinate.
      -- If the x-Short Vector bit is not set and this bit is also not set, the
      -- current x-coordinate is a signed 16-bit delta vector.
    , _flagXSame   :: !Bool
      -- | This flag has two meanings, depending on how the y-Short Vector flag
      -- is set. If y-Short Vector is set, this bit describes the sign of the
      -- value, with 1 equalling positive and 0 negative. If the y-Short Vector
      -- bit is not set and this bit is set, then the current y-coordinate is the
      -- same as the previous y-coordinate. If the y-Short Vector bit is not set
      -- and this bit is also not set, the current y-coordinate is a signed
      -- 16-bit delta vector.
    , _flagYSame   :: !Bool
    }
    deriving (Eq, Show)

instance Binary GlyphFlag where
    put (GlyphFlag a0 a1 a2 a3 a4 a5) =
        putWord8 . foldl setter 0 $ zip [0..] [a0, a1, a2, a3, a4, a5]
      where setter v ( _, False) = v
            setter v (ix, True) = setBit v ix
    get = do
      tester <- testBit <$> getWord8
      return $ GlyphFlag
        { _flagOnCurve = tester 0
        , _flagXshort  = tester 1
        , _flagYShort  = tester 2
        , _flagRepeat  = tester 3
        , _flagXSame   = tester 4
        , _flagYSame   = tester 5
        }

getGlyphFlags :: Int -> Get [GlyphFlag]
getGlyphFlags count = go undefined 0
  where
    go prevFlags n
        | n >= count = return []
        | _flagRepeat prevFlags && n > 0 = do
            repeatCount <- fromIntegral <$> getWord8
            (replicate repeatCount prevFlags ++) <$> go prevFlags (n + repeatCount)
        | otherwise = do
            v <- get
            (v :) <$> go v 1

getCoords :: [GlyphFlag] -> Get (VU.Vector (Int16, Int16))
getCoords flags =
    VU.fromList <$> (zip <$> go (_flagXSame, _flagXshort) 0 flags
                         <*> go (_flagYSame, _flagYShort) 0 flags)
  where
    go _ _ [] = return []
    go axx@(isDual, isShort) prevCoord (flag:flagRest) = do
        newCoord <-
            if isDual flag then
              if isShort flag then (prevCoord +) . fromIntegral <$> getWord8
              else return prevCoord
            else
              if isShort flag then (prevCoord -) . fromIntegral <$> getWord8
              else (prevCoord +) . fromIntegral <$> getWord16be
        (newCoord :) <$> go axx newCoord flagRest

getSimpleOutline :: Int16 -> Get GlyphContent
getSimpleOutline counterCount = do
    endOfPoints <- VU.replicateM (fromIntegral counterCount) getWord16be
    let pointCount = VU.last endOfPoints + 1
    instructionCount <- fromIntegral <$> getWord16be
    instructions <- VU.replicateM instructionCount getWord8

    flags <- getGlyphFlags $ fromIntegral pointCount
    GlyphSimple . GlyphContour instructions <$> getCoords flags

instance Binary Glyph where
    put _ = fail "Glyph.put - unimplemented"
    get = do
      hdr <- get
      case _glfNumberOfContours hdr of
        -1 -> Glyph hdr <$> getCompositeOutline
        n -> Glyph hdr <$> getSimpleOutline n

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
    , _fHdrMacStyle         :: !Word16
      -- | Smallest readable size in pixels.
    , _fHdrLowestRecPPEM     :: !Word16

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

instance Binary FontHeader where
  put _ = fail "Unimplemented"
  get =
    FontHeader <$> get <*> get <*> g32 <*> g32 <*> get
               <*> g16 <*> g64 <*> g64 <*> get <*> get
               <*> get <*> get <*> g16 <*> g16 <*> gi16
               <*> gi16 <*> gi16
      where g16 = getWord16be
            g32 = getWord32be
            gi16 = fromIntegral <$> getWord16be
            g64 = getWord64be


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


data TtfTable
  = RawTable B.ByteString
  deriving (Eq, Show)

data Font = Font
    { fontOffsetTable :: !OffsetTable
    , fontTables      :: ![(B.ByteString, TtfTable)]
    }
    deriving (Eq, Show)

fetchTables :: OffsetTable -> Get [(B.ByteString, TtfTable)]
fetchTables tables = mapM fetch tableList
  where
    tableList = sortBy (compare `on` _tdeOffset)
                    . V.toList
                    $ _otEntries tables
    gotoOffset entry = do
        readed <- bytesRead 
        let toDrop = fromIntegral (_tdeOffset entry) - readed + 1
        if toDrop < 0 then fail "Weirdo weird"
        else skip $ fromIntegral toDrop

    fetch entry 
      | otherwise = do
          let tableLength = fromIntegral $ _tdeLength entry
          gotoOffset entry
          rawData <- getByteString tableLength
          return (_tdeTag entry, RawTable rawData)

instance Binary Font where
  put _ = error "Binary.put Font - unimplemented"
  get = do
    header <- get
    Font header <$> fetchTables header

