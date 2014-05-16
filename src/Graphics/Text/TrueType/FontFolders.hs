
module Graphics.Text.TrueType.FontFolders( loadUnixFontFolderList ) where

import Control.Applicative( (<$>) )
import System.FilePath( (</>) )
import Text.XML.HXT.Core( runX
                        , readDocument
                        , withValidate
                        , withSubstDTDEntities
                        , no
                        , multi
                        , getChildren
                        , isElem
                        , hasName
                        , getText
                        , (>>>) )

import qualified Control.Exception as E

catchAny :: IO a -> (E.SomeException -> IO a) -> IO a
catchAny = E.catch

loadParseFontsConf :: IO [FilePath]
loadParseFontsConf = runX (
        readDocument [withValidate no, withSubstDTDEntities no]
                     "/etc/fonts/fonts.conf"
            >>> multi (isElem >>> hasName "dir" >>> getChildren >>> getText))
 

loadUnixFontFolderList :: IO [FilePath]
loadUnixFontFolderList =
   catchAny (fmap (</> "truetype") <$> loadParseFontsConf)
            (const $ return [])

