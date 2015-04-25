{-# LANGUAGE CPP #-}
module Graphics.Text.TrueType.OffsetTable
    ( OffsetTableHeader( .. )
    , OffsetTable( .. )
    , TableDirectoryEntry( .. )
    , filterTable
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<*>), (<$>) )
#endif

import Data.Word( Word16, Word32 )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( getWord16be
                      , getWord32be
                      , getByteString
                      )

import Data.Binary.Put( putWord16be
                      , putWord32be
                      , putByteString
                      )

import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BC

import Graphics.Text.TrueType.Types

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

filterTable :: (BC.ByteString -> Bool) -> OffsetTable
            -> OffsetTable
filterTable f table =
  table { _otEntries = V.filter (f . _tdeTag) $ _otEntries table }

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

