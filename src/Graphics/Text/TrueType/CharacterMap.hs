module Graphics.Text.TrueType.CharacterMap where

import Control.Monad( replicateM )
import Control.Applicative( (<$>), (<*>) )
import Control.Monad( when )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( Get
                      , bytesRead
                      , getWord8
                      , getWord16be
                      , getWord32be )

import Data.Binary.Put( putWord16be
                      , putWord32be )
import qualified Data.Map as M
import Data.Int( Int16 )
import Data.Word( Word8, Word16, Word32 )

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

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

data CharacterMaps = CharacterMaps [CharacterTable]
    deriving (Eq, Show)

instance Binary CharacterMaps where
  put _ = fail "Unimplemented"
  get = do
    versionNumber <- getWord16be
    when (versionNumber /= 0)
         (fail "Characte map - invalid version number")
    tableCount <- fromIntegral <$> getWord16be
    CharacterMaps <$> replicateM tableCount get

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

data Format4 = Format4
    { _f4Language :: {-# UNPACK #-} !Word16
    , _f4Map :: M.Map Word16 Word16
    }
    deriving (Eq, Show)

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
      idDelta <- fetcher
      idRangeOffset <- fetcher
     
      tableBeginIndex <- bytesRead
      let rangeInfo = init . VU.toList $
              VU.zip5 startCodes endCodes idDelta idRangeOffset $
                   VU.enumFromN 0 segCount

          indexLeft = fromIntegral $
              (tableLength - (tableBeginIndex - startIndex))
                  `div` 2

      indexTable <- VU.replicateM indexLeft getWord16be

      return . Format4 language . M.fromList
             $ concatMap 
                (prepare segCount indexTable) rangeInfo
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
                      (rangeOffset `div` 2) + char - start - ix
                            + (segCount * 2)
                    glyphId = indexTable VU.! index
              ]

data CharacterTable
    = TableFormat0 !(VU.Vector Word8)
    | TableFormat2 !Format2
    | TableFormat4 !Format4
    | TableFormatUnknown !Word16
    deriving (Eq, Show)

getFormat0 :: Get CharacterTable
getFormat0 = TableFormat0 <$> do
    count <- fromIntegral <$> getWord16be
    _version <- getWord16be
    VU.replicateM count getWord8

data Format2SubHeader = Format2SubHeader
    { _f2SubCode       :: {-# UNPACK #-} !Word16
    , _f2EntryCount    :: {-# UNPACK #-} !Word16
    , _f2IdDelta       :: {-# UNPACK #-} !Int16
    , _f2IdRangeOffset :: {-# UNPACK #-} !Word16
    }
    deriving (Eq, Show)

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

data Format2 = Format2
    { _format2Language   :: {-# UNPACK #-} !Word16
    , _format2SubKeys    :: !(VU.Vector Word16)
    , _format2SubHeaders :: !(V.Vector Format2SubHeader)
    }
    deriving (Eq, Show)

instance Binary CharacterTable where
    put _ = fail "Binary.put CharacterTable - Unimplemented"
    get = do
      format <- getWord16be
      case format of
        0 -> getFormat0
        2 -> TableFormat2 <$> get
        4 -> TableFormat4 <$> get
        n -> return $ TableFormatUnknown n

