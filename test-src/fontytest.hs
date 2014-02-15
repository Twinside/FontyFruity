import Data.Maybe( fromJust )
import Data.Monoid( mempty )
{-import Control.Monad( forM_ )-}
import Data.Binary( decodeFile )
import Graphics.Text.TrueType
import qualified Data.Vector as V

import Text.Groom 

main :: IO ()
main = do
    font <- decodeFile "C:/Windows/Fonts/comic.ttf" :: IO Font
    putStrLn . groom
        $ font { _fontGlyph = Nothing
               , _fontTables = [(t, mempty) | (t, _) <- _fontTables font] }
    V.mapM_ (putStrLn . groom) . fromJust $ _fontGlyph font

