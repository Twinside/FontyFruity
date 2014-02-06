module Graphics.Text.TrueType.Types
    ( Fixed
    , FWord
    ) where

import Control.Applicative( (<$>), (<*>) )
import Data.Word( Word16 )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( getWord16be)
import Data.Binary.Put( putWord16be )

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

