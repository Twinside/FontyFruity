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
    , unitsPerEm
    , isPlaceholder
    , getCharacterGlyphsAndMetrics
    , getGlyphForStrings
    , stringBoundingBox
    , findFontOfFamily
    , pointInPixelAtDpi
    , pixelSizeInPointAtDpi

      -- * Font cache
    , FontCache
    , FontDescriptor( .. )
    , emptyFontCache
    , findFontInCache
    , buildCache
    , enumerateFonts
    , descriptorOf

      -- * Types
    , Font
    , FontStyle( .. )
    , RawGlyph( .. )
    , Dpi
    , PointSize( .. )
    , CompositeScaling( .. )
    , BoundingBox( .. )
    ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mempty )
import Control.Applicative((<$>))
#endif

import Control.Monad( foldM, forM )
import Data.Function( on )
import Data.Int ( Int16 )
import Data.List( sortBy, mapAccumL, foldl' )
import Data.Word( Word16 )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( Get
                      , bytesRead
                      , getWord16be
                      , getWord32be
                      , getLazyByteString
                      , skip
                      )

#if MIN_VERSION_binary(0,6,4)
import qualified Data.Binary.Get as G
import Control.DeepSeq( NFData )
#else
import qualified Data.Binary.Get as G
import qualified Control.Exception as E
-- I feel so dirty. :(
import System.IO.Unsafe( unsafePerformIO )
import Control.DeepSeq( NFData, ($!!) )
#endif

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
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
import Graphics.Text.TrueType.Kerning (getKerningValue)
import Graphics.Text.TrueType.Name()
import Graphics.Text.TrueType.FontType
import Graphics.Text.TrueType.FontFolders

{-import Debug.Trace-}

-- | Load a font file, the file path must be pointing
-- to the true type file (.ttf)
loadFontFile :: FilePath -> IO (Either String Font)
loadFontFile filepath = decodeFont <$> LB.readFile filepath

getOrFail :: NFData a => Get a -> LB.ByteString -> Either String a
getOrFail getter str =
#if MIN_VERSION_binary(0,6,4)
  case G.runGetOrFail getter str of
    Left err -> Left $ show err
    Right (_, _, value) -> Right value
#else
  unsafePerformIO $ E.evaluate (let v = G.runGet getter str in
                                return $!! v)
    `E.catch` catcher
      where catcher :: E.SomeException -> IO (Either String a)
            catcher e = return . Left $ show e
#endif

-- | Decode a in-memory true type file.
decodeFont :: LB.ByteString -> Either String Font
decodeFont = getOrFail getFont

decodeWithDefault :: forall a . (NFData a, Binary a)
                  => a -> LB.ByteString -> a
decodeWithDefault defaultValue str =
    case getOrFail get str of
      Left _ -> defaultValue
      Right v -> v

gotoOffset :: TableDirectoryEntry -> Get ()
gotoOffset entry = do
    readed <- bytesRead
    let toDrop = fromIntegral (_tdeOffset entry) - readed
    if toDrop < 0 then fail "Weirdo weird"
    else skip $ fromIntegral toDrop

getLoca :: Font -> Get Font
getLoca font@(Font { _fontMaxp = Just maxp, _fontHeader = Just hdr })
  | _fHdrIndexToLocFormat hdr == 0 = do
      v <- VU.replicateM glyphCount
            ((* 2) . fromIntegral <$> getWord16be)
      return font { _fontLoca = Just v }
  | otherwise = do
      v <- VU.replicateM glyphCount getWord32be
      return font { _fontLoca = Just v }
  where glyphCount = fromIntegral $ _maxpnumGlyphs maxp
getLoca font = return font

getGlyph :: Font -> LB.ByteString -> Get Font
getGlyph font@(Font { _fontLoca = Just locations }) str =

  return font { _fontGlyph = Just . V.map decoder $ VU.convert locationInterval }
      where decoder (xStart, xEnd)
                | xEnd <= xStart = emptyGlyph
                | otherwise =
                    decodeWithDefault emptyGlyph $ chop xStart xEnd
            chop start _ = LB.drop (fromIntegral start) str
            locationsAll = locations `VU.snoc` fromIntegral (LB.length str)
            locationInterval = VU.zip locations $ VU.tail locationsAll
getGlyph font _ = return font

getHmtx :: Font -> Get Font
getHmtx font@Font { _fontMaxp = Just maxp,
                    _fontHorizontalHeader = Just hdr } = do
  let metricCount = _hheaLongHorMetricCount hdr
      glyphCount = fromIntegral $ _maxpnumGlyphs maxp
  table <- getHorizontalMetrics (fromIntegral metricCount) glyphCount
  return font { _fontHorizontalMetrics = Just table }
getHmtx font = return font

fetchTables :: [String] -> OffsetTable -> Get Font
fetchTables tableList offsetTable = do
    let sortedTables =
            sortBy (compare `on` _tdeOffset) . V.toList $ _otEntries offsetTable
    tableData <-
      forM sortedTables $ \entry -> do
          gotoOffset entry
          (B.unpack $ _tdeTag entry,) <$> getLazyByteString (fromIntegral $ _tdeLength entry)

    foldM (fetch tableData) (emptyFont offsetTable) tableList

  where
    getFetch tables name getter =
      case [str | (n, str) <- tables, n == name] of
        [] -> fail $ "Table not found " ++ name
        (s:_) ->
            case getOrFail getter s of
               Left err -> fail err
               Right v -> return v

    fetch tables font "head" = do
      table <- getFetch tables "head" get
      return $ font { _fontHeader = Just table }

    fetch tables font "maxp" = do
      table <- getFetch tables "maxp" get
      return $ font { _fontMaxp = Just table }

    fetch tables font "cmap" = do
      table <- getFetch tables "cmap" get
      return $ font { _fontMap = Just table }

    fetch tables font "name" = do
      table <- getFetch tables "name" get
      return $ font { _fontNames = Just table }

    fetch tables font "hhea" = do
      table <- getFetch tables "hhea" get
      return $ font { _fontHorizontalHeader = Just table }

    fetch tables font "glyf" =
      case [getGlyph font s | ("glyf", s) <- tables] of
        [] -> return font
        (g:_) -> g

    fetch tables font "loca" =
      getFetch tables "loca" (getLoca font)

    fetch tables font "hmtx" =
      getFetch tables "hmtx" (getHmtx font)

    fetch tables font "kern" = do
      table <- getFetch tables "kern" get
      return $ font { _fontKerning = Just table }

    fetch _ font _ = return font

getFont :: Get Font
getFont = get >>= fetchTables allTables
  where
    allTables = ["head", "maxp", "cmap", "name", "hhea",  "loca", "glyf", "hmtx", "kern"]

getFontNameAndStyle :: Get Font
getFontNameAndStyle =
    (filterTable isNecessaryForName <$> get) >>= fetchTables ["head", "name"]
  where
    isNecessaryForName v = v == "name" || v == "head"

-- | This function will search in the system for truetype
-- files and index them in a cache for further fast search.
buildCache :: IO FontCache
buildCache = buildFontCache loader
  where
    loader n =
        toMayb . getOrFail getFontNameAndStyle <$> LB.readFile n
    toMayb (Left _) = Nothing
    toMayb (Right v) = Just v

-- | Try to find a font with the given properties in the
-- font cache.
findFontInCache :: FontCache -> FontDescriptor -> Maybe FilePath
findFontInCache (FontCache cache) descr = M.lookup descr cache

-- | This function will scan the system's font folder to
-- find a font with the desired properties. Favor using
-- a FontCache to speed up the lookup process.
findFontOfFamily :: String -> FontStyle -> IO (Maybe FilePath)
findFontOfFamily = findFont loader
  where
    loader n =
        toMayb . getOrFail getFontNameAndStyle <$> LB.readFile n
    toMayb (Left _) = Nothing
    toMayb (Right v) = Just v

-- | Express device resolution in dot per inch.
type Dpi = Int

-- | Font size expressed in points.
-- You must convert size expressed in pixels to point using
-- the DPI information.
-- See pixelSizeInPointAtDpi
newtype PointSize = PointSize { getPointSize :: Float }
    deriving (Eq, Show)

toPixelCoord :: (Integral a) => Font -> PointSize -> Dpi -> a -> Float
toPixelCoord font pointSize dpi v =
    (fromIntegral v * getPointSize pointSize * fromIntegral dpi) /
    (72 * emSize)
  where
    emSize = fromIntegral $ unitsPerEm font

toFCoord :: Integral a => Font -> PointSize -> Dpi -> Float -> a
toFCoord font size dpi v = floor $ v * emSize / pixelSize
  where
    emSize = fromIntegral $ unitsPerEm font
    pixelSize = size `pointInPixelAtDpi` dpi


pointInPixelAtDpi :: PointSize -> Dpi -> Float
pointInPixelAtDpi size dpi =
    (getPointSize size * fromIntegral dpi) / 72

pixelSizeInPointAtDpi :: Float -> Dpi -> PointSize
pixelSizeInPointAtDpi pixelSize dpi =
    PointSize $ (pixelSize / fromIntegral dpi) * 72

glyphOfStrings :: Font -> String -> [(Glyph, HorizontalMetric)]
glyphOfStrings Font { _fontMap = Just mapping
                    , _fontGlyph = Just glyphes
                    , _fontKerning = Just kernPairs
                    , _fontHorizontalMetrics = Just hmtx } str =
    fetcher <$> glyphIndexPairs
  where
    glyphIndexPairs = zip glyphIndexes (tail glyphIndexes <> [0])
    glyphIndexes = findCharGlyph mapping 0 <$> str
    kerning l r = getKerningValue (fromIntegral l) (fromIntegral r) kernPairs
    kern l r m = m { _hmtxAdvanceWidth = _hmtxAdvanceWidth m + kerning l r }
    metrics = _glyphMetrics hmtx
    fetcher (l, r) = (glyphes V.! l, kern l r (metrics V.! l))
glyphOfStrings _ _ = []

-- | Return the number of pixels relative to the point size.
unitsPerEm :: Font -> Word16
unitsPerEm Font { _fontHeader = Just hdr } =
    fromIntegral $ _fUnitsPerEm hdr
unitsPerEm  _ = 1

-- | String bounding box. with value for
-- min/max.
data BoundingBox = BoundingBox
  { _xMin :: {-# UNPACK #-} !Float
  , _yMin :: {-# UNPACK #-} !Float
  , _xMax :: {-# UNPACK #-} !Float
  , _yMax :: {-# UNPACK #-} !Float
    -- | Should be 0 most of the times.
  , _baselineHeight :: {-# UNPACK #-} !Float
  }
  deriving (Eq)

-- | Compute the bounding box of a string displayed with a font at
-- a given size. The resulting coordinate represent the width and the
-- height in pixels.
stringBoundingBox :: Font -> Dpi -> PointSize -> String -> BoundingBox
stringBoundingBox font dpi size str =
    BoundingBox xMini yMini width yMaxi 0
  where
    glyphs = glyphOfStrings font str
    xMini = case glyphs of
        [] -> 0
        (glyph, _):_ -> toPixel . _glfXMin $ _glyphHeader glyph

    (width, yMini, yMaxi) =
        foldl' go (0, 0, 0) glyphs

    toPixel :: Integral a => a -> Float
    toPixel = toPixelCoord font size dpi

    go (xf, yMin, yMax) (glyph, metric) = (width', yMin', yMax')
      where
        advance = _hmtxAdvanceWidth metric
        width' = xf + toPixel advance
        yMin' = min yMin. toPixel . _glfYMin $ _glyphHeader glyph
        yMax' = max yMax. toPixel . _glfYMax $ _glyphHeader glyph


-- | Extract a list of outlines for every char in the string.
-- The given curves are in an image like coordinate system,
-- with the origin point in the upper left corner.
getStringCurveAtPoint :: Dpi            -- ^ Dot per inch of the output.
                      -> (Float, Float) -- ^ Initial position of the baseline.
                      -> [(Font, PointSize, String)] -- ^ Text to draw
                      -> [[VU.Vector (Float, Float)]] -- ^ List of contours for each char
getStringCurveAtPoint dpi initPos lst = snd $ mapAccumL go initPos glyphes where
  glyphes = concat [(font, size,)
                         <$> glyphOfStrings font str | (font, size, str) <- lst]

  toPixel (font, pointSize, _) = toPixelCoord font pointSize dpi
  toCoord (font, pointSize, _) = toFCoord font pointSize dpi

  maximumSize = maximum [ toPixel p . _glfYMax $ _glyphHeader glyph
                                | p@(_, _, (glyph, _)) <- glyphes ]

  go (xf, yf) p@(font, pointSize, (glyph, metric)) = ((toPixel p $ xi + advance, yf), curves)
    where
      (xi, yi) = (toCoord p xf, toCoord p yf)
      advance = fromIntegral $ _hmtxAdvanceWidth metric
      curves =
          getGlyphIndexCurvesAtPointSizeAndPos font dpi (toCoord p maximumSize)
            (pointSize, glyph) (xi, yi)

-- | This function return the list of all contour for all char with the given
-- font in a string. All glyph are at the same position, they are not placed
-- like with `getStringCurveAtPoint`. It is a function helpful to extract
-- the glyph geometry for further external manipulation.
getGlyphForStrings :: Dpi -> [(Font, PointSize, String)]
                   -> [[VU.Vector (Float, Float)]]
getGlyphForStrings dpi lst =  go <$> glyphes where
  glyphes = concat
    [(font, size,) <$> glyphOfStrings font str | (font, size, str) <- lst]

  toCoord (font, pointSize, _) = toFCoord font pointSize dpi

  maximumSize :: Float
  maximumSize =
     maximum [ toPixelCoord font pointSize dpi . _glfYMax $ _glyphHeader glyph
                   | (font, pointSize, (glyph, _)) <- glyphes ]

  go p@(font, pointSize, (glyph, _metric)) =
    getGlyphIndexCurvesAtPointSizeAndPos
        font dpi (toCoord p maximumSize) (pointSize, glyph) (0, 0)

getGlyphIndexCurvesAtPointSizeAndPos :: Font -> Dpi -> Int -> (PointSize, Glyph)
                                     -> (Int, Int)
                                     -> [VU.Vector (Float, Float)]
getGlyphIndexCurvesAtPointSizeAndPos Font { _fontHeader = Nothing } _ _ _ _ = []
getGlyphIndexCurvesAtPointSizeAndPos Font { _fontGlyph = Nothing } _ _ _ _ = []
getGlyphIndexCurvesAtPointSizeAndPos
    Font { _fontHeader = Just hdr, _fontGlyph = Just allGlyphs }
        dpi _maximumSize (pointSize, topGlyph) (baseX, baseY) = glyphFixup <$> glyphExtract topGlyph
  where
    go index | index >= V.length allGlyphs = []
             | otherwise = glyphExtract $ allGlyphs V.! index

    pixelSize = pointSize `pointInPixelAtDpi` dpi
    emSize = fromIntegral $ _fUnitsPerEm hdr

    glyphFixup = VU.map (\(x,y) ->
                     ( toPixelCoordinate baseX x
                     , toPixelCoordinate baseY (-1 * y)
                     ))

    toFloat v   = fromIntegral v / 0x4000 :: Float
    fromFloat v = truncate (v * 0x4000)

    toPixelCoordinate shift coord =
        (fromIntegral (shift + fromIntegral coord) * pixelSize) / emSize

    composeGlyph base composition = VU.map updateCoords <$> subCurves
      where
        subCurves = go . fromIntegral $ _glyphCompositeIndex composition
        CompositeScaling xx xy yx yy = _glyphCompositionScale composition
        useOffsets                   = _glyphCompositionOffsetArgs composition
        (e,f)                        = _glyphCompositionArg composition

        -- according to a comment in Freetype's source code, the algorithm given
        -- in Apple documentation is wrong. We use the same algorithm as
        -- Freetype except that we go through Float to compute the square root
        -- instead of staying in Fixed 2.14 (or Fixed 16.16 as in Freetype).

        (xx',xy',yx',yy') = (toFloat xx, toFloat xy, toFloat yx, toFloat yy)

        xscale
          | xy == 0   = xx'
          | xx == 0   = xy'
          | otherwise = sqrt (xx' * xx' + xy' * xy')
        yscale
          | yy == 0   = yx'
          | yx == 0   = yy'
          | otherwise = sqrt (yx' * yx' + yy' * yy')

        basePoints = VU.concat (_glyphPoints base)
        (offX,offY)
          | useOffsets = (e,f)
          | otherwise  = (fst b1 - fst b2, snd b1 - snd b2)
               where
                  b1 = basePoints VU.! (fromIntegral e)
                  b2 = basePoints VU.! (fromIntegral f)

        updateCoords (x,y) = ( fromFloat (toFloat x * xscale + toFloat offX)
                             , fromFloat (toFloat y * yscale + toFloat offY)
                             )

    -- recursively find the base contour of a compound glyph
    glyphBase g = case _glyphContent g of
      GlyphEmpty          -> error "Invalid glyph base index"
      GlyphComposite cs _ ->
         glyphBase (allGlyphs V.! fromIntegral (_glyphCompositeIndex (V.head cs)))
      GlyphSimple contour -> contour

    glyphExtract Glyph { _glyphContent = GlyphEmpty } = []
    glyphExtract g@Glyph { _glyphContent = GlyphComposite compositions _ } =
        concatMap (composeGlyph (glyphBase g)) $ V.toList compositions
    glyphExtract Glyph { _glyphContent = GlyphSimple contour } =
        extractFlatOutline contour

-- | True if the character is not present in the font, therefore it
-- will appear as a placeholder in renderings.
isPlaceholder :: Font -> Char -> Bool
isPlaceholder Font { _fontMap = Just fontMap } character =
    findCharGlyph fontMap 0 character == 0
isPlaceholder Font { _fontMap = Nothing } _ = True

-- | Retrive the glyph contours and associated transformations.
-- The coordinate system is assumed to be the TTF one (y upward).
-- No transformation is performed.
getCharacterGlyphsAndMetrics :: Font
                             -> Char
                             -> (Float, V.Vector RawGlyph) -- ^ Advance and glyph information.
getCharacterGlyphsAndMetrics
    Font { _fontMap = Just mapping
         , _fontGlyph = Just allGlyphs
         , _fontHorizontalMetrics = Just hmtx }
        character =
    (advance, getCharacterGlyphs allGlyphs allMetrics glyphIndex)
  where
    glyphIndex = findCharGlyph mapping 0 character
    allMetrics = _glyphMetrics hmtx
    metrics = allMetrics V.! glyphIndex
    advance = fromIntegral (_hmtxAdvanceWidth metrics)
getCharacterGlyphsAndMetrics _ _ = (0, mempty)

-- | This type represent unscaled glyph information, everything
-- is still in its raw form.
data RawGlyph = RawGlyph
    { -- | List of transformations to apply to the contour in order
      -- to get their correct placement.
      _rawGlyphCompositionScale :: ![CompositeScaling]

      -- | Glyph index in the current font.
    , _rawGlyphIndex            :: !Int

      -- | Real Geometry of glyph, each vector contain one
      -- contour.
    , _rawGlyphContour          :: ![VU.Vector (Int16, Int16)]
    }

prependScale :: CompositeScaling -> RawGlyph -> RawGlyph
prependScale scale rGlyph =
    rGlyph { _rawGlyphCompositionScale = scale : _rawGlyphCompositionScale rGlyph }

getCharacterGlyphs :: V.Vector Glyph -> V.Vector HorizontalMetric -> Int
                   -> V.Vector RawGlyph
getCharacterGlyphs allGlyphs allMetrics glyphIndex =
    case _glyphContent (allGlyphs V.! glyphIndex) of
      GlyphEmpty -> mempty
      GlyphComposite compositions _ -> V.concatMap expandComposition compositions
      GlyphSimple countour ->
          V.singleton . RawGlyph mempty glyphIndex $ extractFlatOutline countour
  where
    recurse = getCharacterGlyphs allGlyphs allMetrics 

    expandComposition GlyphComposition { _glyphCompositeIndex = index
                                       , _glyphCompositionScale = scale } =
      V.map (prependScale scale) . recurse $ fromIntegral index

