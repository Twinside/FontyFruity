{-# LANGUAGE RankNTypes #-}
module Graphics.Text.TrueType.CharacterMap
    ( TtfEncoding( .. )
    , CharacterMaps
    , LangId
    , findCharGlyph
    ) where

import Control.Monad( replicateM, forM )
import Control.Applicative( (<$>), (<*>) )
import Control.Monad( when )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( Get
                      , skip
                      , bytesRead
                      , getWord8
                      , getWord16be
                      , getWord32be )

import Data.Binary.Put( putWord16be
                      , putWord32be )
import Data.Int( Int16 )
import Data.List( find, sortBy )
import qualified Data.Map as M
import Data.Maybe( fromMaybe )
import Data.Word( Word8, Word16, Word32 )
import Data.Ord( comparing )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Graphics.Text.TrueType.LanguageIds

{-import Debug.Trace-}
{-import Text.Printf-}

--------------------------------------------------
----            TtfEncoding
--------------------------------------------------
data TtfEncoding
  = EncodingSymbol
  | EncodingUnicode
  | EncodingShiftJIS
  | EncodingBig5
  | EncodingPRC
  | EncodingWansung
  | EncodingJohab
  deriving (Eq, Show)

instance Binary TtfEncoding where
    put EncodingSymbol = putWord16be 0
    put EncodingUnicode = putWord16be 1
    put EncodingShiftJIS = putWord16be 2
    put EncodingBig5 = putWord16be 3
    put EncodingPRC = putWord16be 4
    put EncodingWansung = putWord16be 5
    put EncodingJohab = putWord16be 6

    get = do
      v <- getWord16be
      case v of
        0 -> return EncodingSymbol
        1 -> return EncodingUnicode
        2 -> return EncodingShiftJIS
        3 -> return EncodingBig5
        4 -> return EncodingPRC
        5 -> return EncodingWansung
        6 -> return EncodingJohab
        _ -> fail "Unknown encoding"

class CharMappeable a where
  -- | Given a group a character, return a valid glyph
  -- id and the rest of the string.
  glyphIdFromTable :: a -> Char -> Int

  -- | Given a table, retrieve the language code
  langIdOfCharMap :: a -> LangId

type LangId = Word16

--------------------------------------------------
----            CharacterMaps
--------------------------------------------------
data CharacterMap = CharacterMap
    { _charMapPlatformId       :: !PlatformId
    , _charMapPlatformSpecific :: !Word16
    , _charMap                 :: !CharacterTable
    }
    deriving (Eq, Show)

instance Ord CharacterMap where
    compare = comparing _charMap

newtype CharacterMaps = CharacterMaps [CharacterMap]
    deriving (Eq, Show)

instance Binary CharacterMaps where
  put _ = fail "Unimplemented"
  get = do
    startIndex <- bytesRead
    versionNumber <- getWord16be
    when (versionNumber /= 0)
         (fail "Characte map - invalid version number")
    tableCount <- fromIntegral <$> getWord16be
    tableDesc <- replicateM tableCount $
        (,,) <$> get <*> getWord16be <*> getWord32be

    tables <-
      forM tableDesc $ \(platformId, platformSpecific, offset) -> do
         currentOffset <- fromIntegral <$> bytesRead
         let toSkip = fromIntegral offset - currentOffset + startIndex
         when (toSkip > 0)
              (skip $ fromIntegral toSkip)
         CharacterMap platformId platformSpecific <$> get
    return . CharacterMaps $ sortBy (comparing _charMap) tables

data CharMapOffset = CharMapOffset
    { _cmoPlatformId :: !Word16
    , _cmoEncodingId :: !TtfEncoding
    , _cmoOffset     :: !Word32
    }
    deriving (Eq, Show)

instance Binary CharMapOffset where
    get = CharMapOffset <$> getWord16be <*> get <*> getWord32be
    put (CharMapOffset platform encoding offset) =
      putWord16be platform >> put encoding >> putWord32be offset

--------------------------------------------------
----            CharacterTable
--------------------------------------------------
data CharacterTable
    = TableFormat0 !Format0
    | TableFormat2 !Format2
    | TableFormat4 !Format4
    | TableFormat6 !Format6
    | TableFormatUnknown !Word16
    deriving (Eq, Show)

charTableMap :: (forall table . (CharMappeable table) => table -> a)
             -> CharacterTable -> a
charTableMap f = go
  where
    go (TableFormat0 t) = f t
    go (TableFormat2 t) = f t
    go (TableFormat4 t) = f t
    go (TableFormat6 t) = f t
    go (TableFormatUnknown v) = f v

findCharGlyph :: CharacterMaps -> LangId -> Char -> Int
findCharGlyph (CharacterMaps charMaps) langId character =
    fromMaybe 0 $ find (/= 0)
        [charTableMap (flip glyphIdFromTable character) m
                | allMap <- charMaps
                , let m = _charMap allMap
                , isLangCompatible m]
  where
    isLangCompatible v = tableLang == 0 || tableLang == langId 
      where tableLang = charTableMap langIdOfCharMap v

instance Ord CharacterTable where
    compare (TableFormat0 v1) (TableFormat0 v2) =
        (comparing langIdOfCharMap) v1 v2
    compare (TableFormat2 v1) (TableFormat2 v2) =
        (comparing langIdOfCharMap) v1 v2
    compare (TableFormat4 v1) (TableFormat4 v2) =
        (comparing langIdOfCharMap) v1 v2
    compare (TableFormat6 v1) (TableFormat6 v2) =
        (comparing langIdOfCharMap) v1 v2
    compare (TableFormat0 _) _ = LT
    compare (TableFormat2 _) _ = LT
    compare (TableFormat4 _) _ = LT
    compare (TableFormat6 _) _ = LT
    compare _ _ = GT

instance Binary CharacterTable where
    put _ = fail "Binary.put CharacterTable - Unimplemented"
    get = do
      format <- getWord16be
      case format of
        0 -> TableFormat0 <$> get
        2 -> TableFormat2 <$> get
        4 -> TableFormat4 <$> get
        6 -> TableFormat6 <$> get
        n -> return $ TableFormatUnknown n

instance CharMappeable Word16 where
  glyphIdFromTable _ _ = 0
  langIdOfCharMap _ = 0


--------------------------------------------------
----            Format4
--------------------------------------------------
data Format4 = Format4
    { _f4Language :: {-# UNPACK #-} !LangId
    , _f4Map :: M.Map Word16 Word16
    }
    deriving (Eq, Show)

instance CharMappeable Format4 where
  glyphIdFromTable tab v =
      fromIntegral . M.findWithDefault 0 wc $ _f4Map tab
        where wc = fromIntegral $ fromEnum v
  langIdOfCharMap = _f4Language

instance Binary Format4 where
  put _ = error "put Format4 - unimplemented"
  get = do
      startIndex <- bytesRead
      tableLength <- fromIntegral <$> getWord16be
      language <- getWord16be
      -- 2 * segCount
      segCount <- (`div` 2) . fromIntegral <$> getWord16be
      -- 2 * (2**FLOOR(log2(segCount)))
      _searchRange <- getWord16be
      -- log2(searchRange/2)
      _entrySelector <- getWord16be
      -- (2 * segCount) - searchRange
      _rangeShift <- getWord16be

      let fetcher :: Get (VU.Vector Int)
          fetcher =
            VU.replicateM segCount (fromIntegral <$> getWord16be)

      endCodes <- fetcher
      _reservedPad <- getWord16be
      startCodes <- fetcher
      idDelta <-
          VU.replicateM segCount (fromIntegral <$> getWord16be) :: Get (VU.Vector Int16)
      idRangeOffset <- fetcher

      tableBeginIndex <- bytesRead
      let idDeltaInt = VU.map fromIntegral idDelta
          rangeInfo = init . VU.toList $
              VU.zip5 startCodes endCodes idDeltaInt idRangeOffset $
                   VU.enumFromN 0 segCount

          indexLeft = fromIntegral $
              (tableLength - (tableBeginIndex - startIndex + 2))
                  `div` 2

      indexTable <- VU.replicateM indexLeft getWord16be

      return . Format4 language . M.fromList
             $ concatMap (prepare segCount indexTable) rangeInfo
    where
      prepare _ _ (start, end, delta, 0, _) =
        [(fromIntegral $ char, fromIntegral $ char + delta)
                    | char <- [start .. end]]
      prepare segCount indexTable
              (start, end, delta, rangeOffset, ix) =
        -- this is... so convoluted oO
        [( fromIntegral char
         , fromIntegral (if glyphId == 0 then 0 else glyphId + fromIntegral delta))
              | char <- [start .. end]
              , let index =
                      (rangeOffset `div` 2) + (char - start) + ix
                            - segCount
              , index < VU.length indexTable
              , let glyphId = indexTable VU.! index
              ]

--------------------------------------------------
----            Format0
--------------------------------------------------
data Format0 = Format0
    { _format0Language :: {-# UNPACK #-} !LangId
    , _format0Table    :: !(VU.Vector Word8)
    }
    deriving (Eq, Show)

instance CharMappeable Format0 where
  glyphIdFromTable Format0 { _format0Table = table } v
    | ic > VU.length table = 0
    | otherwise = fromIntegral $ table VU.! ic
        where ic = fromEnum v
        
  langIdOfCharMap = _format0Language

instance Binary Format0 where
    put _ = fail "Binary.Format0.put - unimplemented"
    get = do
        tableSize <- getWord16be
        when (tableSize /= 262) $
            fail "table cmap format 0 : invalid size"
        Format0 <$> getWord16be <*> VU.replicateM 256 getWord8

--------------------------------------------------
----            Format2
--------------------------------------------------
data Format2 = Format2
    { _format2Language   :: {-# UNPACK #-} !LangId
    , _format2SubKeys    :: !(VU.Vector Word16)
    , _format2SubHeaders :: !(V.Vector Format2SubHeader)
    }
    deriving (Eq, Show)

data Format2SubHeader = Format2SubHeader
    { _f2SubCode       :: {-# UNPACK #-} !Word16
    , _f2EntryCount    :: {-# UNPACK #-} !Word16
    , _f2IdDelta       :: {-# UNPACK #-} !Int16
    , _f2IdRangeOffset :: {-# UNPACK #-} !Word16
    }
    deriving (Eq, Show)

instance CharMappeable Format2 where
  glyphIdFromTable _ _ = 0
  langIdOfCharMap = _format2Language

instance Binary Format2SubHeader where
    put (Format2SubHeader a b c d) =
        p16 a >> p16 b >> pi16 c >> p16 d
      where
        p16 = putWord16be
        pi16 = p16 . fromIntegral

    get = Format2SubHeader <$> g16 <*> g16 <*> (fromIntegral <$> g16) <*> g16
      where g16 = getWord16be


instance Binary Format2 where
    put _ = fail "Format2.put - unimplemented"
    get = do
      _tableSize <- getWord16be
      lang <- getWord16be
      subKeys <- VU.map (`div` 8) <$> VU.replicateM 256 getWord16be
      let maxSubIndex = VU.maximum subKeys
      subHeaders <- V.replicateM (fromIntegral maxSubIndex) get
      -- TODO finish the parsing of format 2
      return $ Format2 lang subKeys subHeaders

--------------------------------------------------
----            Format6
--------------------------------------------------
data Format6 = Format6
    { _format6Language   :: {-# UNPACK #-} !LangId
    , _format6FirstCode  :: {-# UNPACK #-} !Word16
    , _format6ArrayIndex :: !(VU.Vector Word16)
    }
    deriving (Eq, Show)

instance CharMappeable Format6 where
  glyphIdFromTable Format6 { _format6ArrayIndex = table } v
    | ic < VU.length table = fromIntegral $ table VU.! ic
    | otherwise = 0
      where ic = fromEnum v

  langIdOfCharMap = _format6Language

instance Binary Format6 where
    put _ = fail "Format6.put - unimplemented"
    get = do
        _length <- getWord16be
        language <- getWord16be
        firstCode <- getWord16be
        entryCount <- fromIntegral <$> getWord16be
        Format6 language firstCode <$> VU.replicateM entryCount getWord16be
