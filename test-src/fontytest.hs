import Control.Monad( forM_ )
import Data.Binary( decodeFile )
import Graphics.Text.TrueType
import qualified Data.Vector as V

main :: IO ()
main = do
    table <- decodeFile "C:/Windows/Fonts/Consola.ttf" :: IO OffsetTable
    print $ _otHeader table
    V.forM_ (_otEntries table) print

