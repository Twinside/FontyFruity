import Control.Monad( forM_ )
import Data.Binary( decodeFile )
import Graphics.Text.TrueType
import qualified Data.Vector as V

main :: IO ()
main = do
    font <- decodeFile "C:/Windows/Fonts/Consola.ttf" :: IO Font
    print font

