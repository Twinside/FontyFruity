import Data.Maybe( fromJust )
import Data.Monoid( mempty )
{-import Control.Monad( forM_ )-}
import Data.Binary( decodeFile )
import qualified Data.Vector as V
import System.Environment( getArgs )

import Graphics.Text.TrueType
import Graphics.Text.TrueType.Internal

dumpFont :: Font -> IO ()
dumpFont font = do
  putStrLn . show $ font
    { _fontGlyph = Nothing
    , _fontTables = [(t, mempty) | (t, _) <- _fontTables font] }
  V.mapM_ (putStrLn . show) . fromJust $ _fontGlyph font

dumpFontName :: FilePath -> IO ()
dumpFontName fontname = do
  font <- loadFontFile fontname
  case font of
    Left err -> putStrLn err
    Right font -> dumpFont font

main :: IO ()
main = do
  args <- getArgs
  o <- findFontOfFamily "Consolas" $ FontStyle False False
  case o of
    Nothing -> putStrLn "not found"
    Just v -> putStrLn v

  case args of
    [] -> putStrLn "missing font filename"
    (fontname:_) -> dumpFontName fontname

