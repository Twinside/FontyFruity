{-# LANGUAGE CPP #-}
module Graphics.Text.TrueType.Name
    ( NameTable
    , NameRecords
    , fontFamilyName
    ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mempty )
import Control.Applicative( (<$>), (<*>), pure )
#endif

import Control.DeepSeq( NFData( .. ) )
import Control.Monad( when, replicateM )
import Data.Foldable( asum )
import Data.Function( on )
import Data.List( maximumBy )
import Data.Maybe( fromMaybe )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( getWord16be, getByteString )
import Data.Binary.Put( putWord16be )
import Data.Word( Word16 )
import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Graphics.Text.TrueType.LanguageIds

data NameTable = NameTable
    { _ntRecords      :: !(V.Vector NameRecords) }
    deriving Show

instance NFData NameTable where
    rnf (NameTable {}) = ()

instance Binary NameTable where
  put _ = error "Binary.put NameTable - unimplemented"
  get = do
    nameFormatId <- getWord16be
    when (nameFormatId /= 0) $
          fail "Invalid name table format"
    count <- getWord16be
    stringOffset <- getWord16be
    records <- replicateM (fromIntegral count) get
    let maxRec = maximumBy (compare `on` _nrOffset) records
        toFetch = fromIntegral (_nrOffset maxRec) + fromIntegral (_nrLength maxRec)
                - fromIntegral stringOffset

    strTable <- getByteString toFetch
    let fetcher r = r { _nrString = B.take iLength $ B.drop iOffset strTable }
          where iLength = fromIntegral $ _nrLength r
                iOffset = fromIntegral $ _nrOffset r

    return . NameTable . V.fromListN (fromIntegral count) $ map fetcher records

data NameRecords = NameRecords
    { _nrPlatoformId        :: {-# UNPACK #-} !Word16
    , _nrPlatformSpecificId :: {-# UNPACK #-} !Word16
    , _nrLanguageId         :: {-# UNPACK #-} !Word16
    , _nrNameId             :: {-# UNPACK #-} !Word16
    , _nrLength             :: {-# UNPACK #-} !Word16
    , _nrOffset             :: {-# UNPACK #-} !Word16
    , _nrString             :: !B.ByteString
    }
    deriving Show

fontFamilyName :: NameTable -> T.Text
fontFamilyName (NameTable { _ntRecords = records }) =
    fromMaybe T.empty . asum $ transform <$>
            [ (selectorUnicode, utf16Decoder)
            , (selectorMac, utf8Decoder)
            , (selectorWin0, utf16Decoder)
            , (selectorWin1, utf16Decoder)
            ]
  where
    utf16Decoder = TE.decodeUtf16BE . _nrString
    utf8Decoder = TE.decodeUtf8 . _nrString
    transform (selector, decoder) =
        decoder <$> V.find selector records

    fontFamilyId = 1

    windowsPlatform =
        platformToWord PlatformWindows
    selectorWin0 r =
                  _nrNameId r == fontFamilyId &&
      _nrPlatoformId r        == windowsPlatform &&
      _nrPlatformSpecificId r == 0

    selectorWin1 r =
                  _nrNameId r == fontFamilyId &&
      _nrPlatoformId r        == windowsPlatform &&
      _nrPlatformSpecificId r == 1

    macPlatform =
        platformToWord PlatformMacintosh
    selectorMac r =
                  _nrNameId r == fontFamilyId &&
      _nrPlatoformId r        == macPlatform &&
      _nrPlatformSpecificId r == 0

    unicodePlatform =
        platformToWord PlatformUnicode
    semanticUnicode2 =
        unicodePlatformSpecificToId UnicodeBMPOnly2_0
    selectorUnicode r =
                  _nrNameId r == fontFamilyId &&
      _nrPlatoformId r        == unicodePlatform &&
      _nrPlatformSpecificId r == semanticUnicode2

instance Binary NameRecords where
  get = NameRecords <$> g16 <*> g16 <*> g16
                    <*> g16 <*> g16 <*> g16
                    <*> pure mempty
      where g16 = getWord16be

  put (NameRecords p ps l n len ofs _) =
      p16 p >> p16 ps >> p16 l >> p16 n >> p16 len >> p16 ofs
    where p16 = putWord16be
