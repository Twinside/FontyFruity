{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Module in charge of loading fonts.
module Graphics.Text.TrueType
    ( -- * Functions
      decodeFont
    , loadFontFile
    , getStringCurveAtPoint

      -- * Types
    , Font( .. )
    , Dpi
    , PointSize
    ) where

import Control.Applicative( (<$>) )
import Control.Monad( foldM )
import Data.Function( on )
import Data.List( sortBy, mapAccumL )
import Data.Word( Word32 )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( Get
                      , bytesRead
                      , getWord16be
                      , getWord32be
                      , getByteString
                      , getLazyByteString
                      , skip
                      )

#if MIN_VERSION_binary(0,6,4)
import qualified Data.Binary.Get as G
#else
import qualified Data.Binary.Get as G
import qualified Data.Binary as DB
import qualified Control.Exception as E
-- I feel so dirty. :(
import System.IO.Unsafe( unsafePerformIO )
#endif

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

{-import Graphics.Text.TrueType.Types-}
import Graphics.Text.TrueType.MaxpTable
import Graphics.Text.TrueType.Glyph
import Graphics.Text.TrueType.Header
import Graphics.Text.TrueType.OffsetTable
import Graphics.Text.TrueType.CharacterMap
import Graphics.Text.TrueType.HorizontalInfo

{-import Debug.Trace-}

-- | Type representing a font.
data Font = Font
    { _fontOffsetTable       :: !OffsetTable
    , _fontTables            :: ![(B.ByteString, B.ByteString)]
    , _fontHeader            :: Maybe FontHeader
    , _fontMaxp              :: Maybe MaxpTable
    , _fontMap               :: Maybe CharacterMaps
    , _fontGlyph             :: Maybe (V.Vector Glyph)
    , _fontLoca              :: Maybe (VU.Vector Word32)
    , _fontHorizontalHeader  :: Maybe HorizontalHeader
    , _fontHorizontalMetrics :: Maybe HorizontalMetricsTable
    }
    deriving (Show)

emptyFont :: OffsetTable -> Font
emptyFont table = Font
    { _fontTables            = []
    , _fontOffsetTable       = table
    , _fontHeader            = Nothing
    , _fontGlyph             = Nothing
    , _fontMaxp              = Nothing
    , _fontLoca              = Nothing
    , _fontMap               = Nothing
    , _fontHorizontalHeader  = Nothing
    , _fontHorizontalMetrics = Nothing
    }

-- | Load a font file, the file path must be pointing
-- to the true type file (.ttf)
loadFontFile :: FilePath -> IO (Either String Font)
loadFontFile filepath = decodeFont <$> LB.readFile filepath

-- | Decode a in-memory true type file.
decodeFont :: LB.ByteString -> Either String Font
decodeFont str =
#if MIN_VERSION_binary(0,6,4)
  case G.runGetOrFail getFont str of
    Left err -> Left $ show err
    Right (_, _, value) -> Right value
#else
  unsafePerformIO $ E.evaluate (return $ G.runGet getFont str)
    `E.catch` catcher
      where catcher :: E.SomeException -> IO (Either String a)
            catcher e = return . Left $ show e
#endif

decodeWithDefault :: forall a . Binary a => a -> LB.ByteString -> a
decodeWithDefault defaultValue str =
#if MIN_VERSION_binary(0,6,4)
  case G.runGetOrFail get str of
    Left _ -> defaultValue
    Right (_, _, value) -> value
#else
  unsafePerformIO $ E.evaluate (DB.decode str) `E.catch` catcher
      where catcher :: E.SomeException -> IO a
            catcher _ = return defaultValue
#endif

fetchTables :: OffsetTable -> Get Font
fetchTables tables = foldM fetch (emptyFont tables) tableList
  where
    tableList = sortBy (compare `on` _tdeOffset)
                    . V.toList
                    $ _otEntries tables
    gotoOffset entry = do
        readed <- bytesRead 
        let toDrop = fromIntegral (_tdeOffset entry) - readed
        if toDrop < 0 then fail "Weirdo weird"
        else skip $ fromIntegral toDrop

    getLoca font@(Font { _fontMaxp = Just maxp, _fontHeader = Just hdr })
      | _fHdrIndexToLocFormat hdr == 0 = do
          v <- VU.replicateM glyphCount
                ((* 2) . fromIntegral <$> getWord16be)
          return $ font { _fontLoca = Just v }
      | otherwise = do
          v <- VU.replicateM glyphCount getWord32be
          return $ font { _fontLoca = Just v }
      where glyphCount = fromIntegral $ _maxpnumGlyphs maxp
    getLoca font = return font

    getGlyph font@(Font { _fontLoca = Just locations }) str =
      return $ font { _fontGlyph = Just . V.map decoder $ VU.convert locationInterval }
          where decoder (xStart, xEnd)
                    | xEnd <= xStart = emptyGlyph
                    | otherwise = 
                        decodeWithDefault emptyGlyph $ chop xStart xEnd
                chop start _ = LB.drop (fromIntegral start) str
                locationsAll = locations `VU.snoc` (fromIntegral $ LB.length str)
                locationInterval = VU.zip locations $ VU.tail locationsAll

    getGlyph font _ = return font

    fetch font entry | _tdeTag entry == "loca" =
      gotoOffset entry >> getLoca font

    fetch font entry | _tdeTag entry == "glyf" =
      gotoOffset entry >>
          getLazyByteString (fromIntegral $ _tdeLength entry)
                >>= getGlyph font

    fetch font entry | _tdeTag entry == "head" = do
      table <- gotoOffset entry >> get
      return $ font { _fontHeader = Just table }

    fetch font entry | _tdeTag entry == "maxp" = do
      table <- gotoOffset entry >> get
      return $ font { _fontMaxp = Just table }

    fetch font entry | _tdeTag entry == "cmap" = do
      table <- gotoOffset entry >> get
      return $ font { _fontMap = Just table }

    fetch font entry | _tdeTag entry == "hhea" = do
      table <- gotoOffset entry >> get
      return $ font { _fontHorizontalHeader = Just table }
    fetch font@Font { _fontMaxp = Just maxp,
                      _fontHorizontalHeader = Just hdr } entry
        | _tdeTag entry == "hmtx" = do
      gotoOffset entry
      let metricCount = _hheaLongHorMetricCount hdr
      table <- getHorizontalMetrics (fromIntegral metricCount) glyphCount
      return font { _fontHorizontalMetrics = Just table }
     where glyphCount = fromIntegral $ _maxpnumGlyphs maxp

    fetch font entry = do
      let tableLength = fromIntegral $ _tdeLength entry
      rawData <- gotoOffset entry >> getByteString tableLength
      return $ font { _fontTables =
                        (_tdeTag entry, rawData) : _fontTables font}

getFont :: Get Font
getFont = get >>= fetchTables

-- | Express device resolution in dot per inch.
type Dpi = Int

-- | Font size expressed in points.
type PointSize = Int

glyphOfStrings :: Font -> String -> [(Glyph, HorizontalMetric)]
glyphOfStrings Font { _fontMap = Just mapping
                    , _fontGlyph = Just glyphes
                    , _fontHorizontalMetrics = Just hmtx } str = fetcher . findCharGlyph mapping 0 <$> str
  where
    fetcher ix = (glyphes V.! ix, _glyphMetrics hmtx V.! ix)
glyphOfStrings _ _ = []

-- | Extract a list of outlines for every char in the string.
-- The given curves are in an image like coordinate system,
-- with the origin point in the upper left corner.
getStringCurveAtPoint :: Dpi            -- ^ Dot per inch of the output.
                      -> (Float, Float) -- ^ Initial position of the baseline.
                      -> [(Font, PointSize, String)] -- ^ Text to draw
                      -> [[VU.Vector (Float, Float)]] -- ^ List of contours for each char
getStringCurveAtPoint dpi initPos lst = snd $ mapAccumL go initPos glyphes where 
  glyphes = concat [ (font, size, unitsPerEm font,) <$> glyphOfStrings font str | (font, size, str) <- lst]

  unitsPerEm Font { _fontHeader = Just hdr } = fromIntegral $ _fUnitsPerEm hdr
  unitsPerEm  _ = 1

  toPixel (_, pointSize, emSize, _) v = fromIntegral v * pixelSize / emSize
    where
      pixelSize = fromIntegral (pointSize * dpi) / 72

  toFCoord (_, pointSize, emSize, _) v = floor $ v * emSize / pixelSize
    where
      pixelSize = fromIntegral (pointSize * dpi) / 72

  maximumSize = maximum [ toPixel p . _glfYMax $ _glyphHeader glyph
                                | p@(_, _, _, (glyph, _)) <- glyphes ]

  go (xf, yf) p@(font, pointSize, _, (glyph, metric)) = ((toPixel p $ xi + advance, yf), curves)
    where
      (xi, yi) = (toFCoord p xf, toFCoord p yf)
      bearing = fromIntegral $ _hmtxLeftSideBearing metric
      advance = fromIntegral $ _hmtxAdvanceWidth metric
      curves =
          getGlyphIndexCurvesAtPointSizeAndPos font dpi (toFCoord p maximumSize)
            (pointSize, glyph) (xi + bearing, yi)


getGlyphIndexCurvesAtPointSizeAndPos :: Font -> Dpi -> Int -> (PointSize, Glyph) -> (Int, Int)
                                     -> [VU.Vector (Float, Float)]
getGlyphIndexCurvesAtPointSizeAndPos Font { _fontHeader = Nothing } _ _ _ _ = []
getGlyphIndexCurvesAtPointSizeAndPos Font { _fontGlyph = Nothing } _ _ _ _ = []
getGlyphIndexCurvesAtPointSizeAndPos
    Font { _fontHeader = Just hdr, _fontGlyph = Just allGlyphs }
        dpi maximumSize (pointSize, topGlyph) (baseX, baseY) = glyphReverse <$> glyphExtract topGlyph
  where
    go index | index >= V.length allGlyphs = []
             | otherwise = glyphExtract $ allGlyphs V.! index

    pixelSize = fromIntegral (pointSize * dpi) / 72
    emSize = fromIntegral $ _fUnitsPerEm hdr

    maxiF = toPixelCoordinate (0 :: Int) maximumSize
    baseYF = toPixelCoordinate (0 :: Int) baseY

    glyphReverse = VU.map (\(x,y) -> (x, maxiF - y + baseYF))

    toPixelCoordinate shift coord =
        (fromIntegral (shift + fromIntegral coord) * pixelSize) / emSize

    composeGlyph composition = VU.map updateCoords <$> subCurves
      where
        subCurves = go . fromIntegral $ _glyphCompositeIndex composition
        toFloat v = fromIntegral v / (0x4000 :: Float)
        CompositeScaling ai bi ci di ei fi = _glyphCompositionScale composition

        scaler v1 v2
            | fromIntegral (abs (abs ai - abs ci)) <= (33 / 65536 :: Float) = 2 * vf
            | otherwise = vf
          where
           vf = toFloat $ max (abs v1) (abs v2)

        m = scaler ai bi
        n = scaler ci di

        am = toFloat ai / m
        cm = toFloat ci / m
        bn = toFloat ci / n
        dn = toFloat di / n
        e = toFloat ei
        f = toFloat fi

        updateCoords (x,y) =
            (m * (am * x + cm *y + e), n * (bn * x + dn * y + f))

    glyphExtract Glyph { _glyphContent = GlyphEmpty } = []
    glyphExtract Glyph { _glyphContent = GlyphComposite compositions _ } =
        concatMap composeGlyph $ V.toList compositions
    glyphExtract Glyph { _glyphContent = GlyphSimple countour } =
        [ VU.map (\(x, y) -> (toPixelCoordinate baseX x, toPixelCoordinate (0 :: Int) y)) c
                | c <- extractFlatOutline countour]

