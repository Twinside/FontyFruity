module Graphics.Text.TrueType.Name
    ( NameTable
    , NameRecords
    ) where

import Control.Applicative( (<$>), (<*>), pure )
import Control.Monad( when, replicateM )
import Data.Function( on )
import Data.List( maximumBy )
import Data.Monoid( mempty )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( getWord16be, getByteString )
import Data.Binary.Put( putWord16be )
import Data.Word( Word16 )
import qualified Data.Vector as V
import qualified Data.ByteString as B

data NameTable = NameTable
    { _ntRecords      :: !(V.Vector NameRecords) }
    deriving Show

instance Binary NameTable where
  put _ = fail "Binary.put NameTable - unimplemented"
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

instance Binary NameRecords where
  get = NameRecords <$> g16 <*> g16 <*> g16 
                    <*> g16 <*> g16 <*> g16 
                    <*> pure mempty
      where g16 = getWord16be

  put (NameRecords p ps l n len ofs _) =
      p16 p >> p16 ps >> p16 l >> p16 n >> p16 len >> p16 ofs
    where p16 = putWord16be


