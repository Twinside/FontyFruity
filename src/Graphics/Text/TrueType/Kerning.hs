{-# LANGUAGE CPP #-}
module Graphics.Text.TrueType.Kerning
  ( KernTable
  , getKerningValue
  ) where

import Control.DeepSeq( NFData( .. ) )
import Control.Monad (replicateM, when)
import Data.Bits( (.&.) )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( Get
                      , getWord16be
                      , getInt16be
                      , skip
                      )
import Data.Int( Int16 )
import qualified Data.Map.Strict as M
import Data.Maybe( fromMaybe )
import Data.Word( Word16 )

data KernTable = KernTable
  { _kernPairs :: M.Map (Word16, Word16) Int16 } 
  deriving (Eq, Show)

instance NFData KernTable where
  rnf (KernTable {}) = ()

instance Binary KernTable where
  put = error "Binary.put KernTable - unimplemented"
  get = getKern

getKern :: Get KernTable
getKern = do
  version <- getWord16be
  case version of
    0 -> getKernV0
    1 -> getKernV1
    v -> fail $ "Unsupported kern table version " <> show v

-- Windows kern table
getKernV0 :: Get KernTable
getKernV0 = do
  skip 2 -- skip nTables
  subtableVersion <- getWord16be 
  when (subtableVersion /= 0) $
    fail $ "Unsupported kern subtable version " <> show subtableVersion
  skip 4 -- skip subtableLength, subtableCoverage
  nPairs <- getWord16be
  skip 6 -- skip searchRange, entrySelector, rangeShift
  pairs <- replicateM (fromIntegral nPairs) getPair
  return $ KernTable $ M.fromList pairs

-- Mac kern table
getKernV1 :: Get KernTable
getKernV1 = do
  skip 2 -- skip 16 bits because version 1 uses 32-bit version but we only consumed 16.
  skip 4 -- skip nTables
  skip 4 -- skip length of subtable
  coverage <- getWord16be
  let subtableVersion = coverage .&. 0x00FF
  skip 2 -- skip tupleIndex subtable header
  if subtableVersion == 0
    then do
      nPairs <- getWord16be
      skip 6 -- skip searchRange, entrySelector, rangeShift
      pairs <- replicateM (fromIntegral nPairs) getPair
      return $ KernTable $ M.fromList pairs
    else return $ KernTable mempty

getPair :: Get ((Word16, Word16), Int16)
getPair = do
  left <- getWord16be
  right <- getWord16be
  value <- getInt16be
  return ((left, right), value)

getKerningValue
  :: (Integral v)
  => Word16 -- left glyph index
  -> Word16 -- right glyph index
  -> KernTable
  -> v -- value added to advance width
getKerningValue l r = 
  fromIntegral . fromMaybe 0 . M.lookup (l, r) . _kernPairs
