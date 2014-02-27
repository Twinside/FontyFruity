module Graphics.Text.TrueType.MaxpTable( MaxpTable( .. ) ) where

import Control.Applicative( (<$>), (<*>) )
import Data.Word( Word16 )
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( getWord16be )

import Graphics.Text.TrueType.Types

data MaxpTable = MaxpTable 
    { -- | version number	0x00010000 for version 1.0.
      _maxpTableVersion :: !Fixed
    -- | The number of glyphs in the font.
    , _maxpnumGlyphs :: !Word16
    -- | Maximum points in a non-composite glyph.
    , _maxpmaxPoints :: !Word16
    -- | Maximum contours in a non-composite glyph.
    , _maxpmaxContours :: !Word16
    -- | Maximum points in a composite glyph.
    , _maxpmaxCompositePoints :: !Word16
    -- | Maximum contours in a composite glyph.
    , _maxpmaxCompositeContours :: !Word16
    -- | 1 if instructions do not use the twilight zone (Z0), or 2 if instructions do use Z0; should be set to 2 in most cases.
    , _maxpmaxZones :: !Word16
    -- | Maximum points used in Z0.
    , _maxpmaxTwilightPoints :: !Word16
    -- | Number of Storage Area locations. 
    , _maxpmaxStorage :: !Word16
    -- | Number of FDEFs.
    , _maxpmaxFunctionDefs :: !Word16
    -- | Number of IDEFs.
    , _maxpmaxInstructionDefs :: !Word16
    -- | Maximum stack depth .
    , _maxpmaxStackElements :: !Word16
    -- | Maximum byte count for glyph instructions.
    , _maxpmaxSizeOfInstructions :: !Word16
    -- | Maximum number of components referenced at "top level" for any composite glyph.
    , _maxpmaxComponentElements :: !Word16
    -- | Maximum levels of recursion; 1 for simple components.
    , _maxpmaxComponentDepth :: !Word16
    }
    deriving (Eq, Show)

instance Binary MaxpTable where
    put _ = fail "Unimplemented"
    get = MaxpTable 
       <$> get <*> g16 <*> g16 <*> g16 <*> g16 <*> g16
       <*> g16 <*> g16 <*> g16 <*> g16 <*> g16 <*> g16
       <*> g16 <*> g16 <*> g16
         where g16 = getWord16be

