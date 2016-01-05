{-# LANGUAGE CPP #-}
module Graphics.Text.TrueType.Types
    ( Fixed( .. )
    , FWord( .. )
    , F26Dot6( .. )
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>), (<*>) )
#endif

import Data.Int( Int32 )
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

newtype F26Dot6 = F26Dot6 Int32
    deriving (Eq, Show, Ord)

instance Num F26Dot6 where
    (F26Dot6 a) + (F26Dot6 b) = F26Dot6 $ a + b
    (F26Dot6 a) - (F26Dot6 b) = F26Dot6 $ a - b
    (F26Dot6 a) * (F26Dot6 b) = F26Dot6 $ (a * b) `div` 64
    negate (F26Dot6 a) = F26Dot6 $ negate a
    abs (F26Dot6 a) = F26Dot6 $ abs a
    signum (F26Dot6 a) = F26Dot6 $ signum a * 64
    fromInteger a = F26Dot6 $ fromInteger a * 64

instance Fractional F26Dot6 where
    fromRational a = F26Dot6 $ floor ad
      where ad = fromRational a * 64 :: Double
    (F26Dot6 a) / (F26Dot6 b) = F26Dot6 $ (a * 64) `div` b

