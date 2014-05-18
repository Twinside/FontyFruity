
module Graphics.Text.TrueType.FontFolders
    ( loadUnixFontFolderList
    , loadWindowsFontFolderList
    , fontFolders
    , findFont
    ) where

import Control.Applicative( (<$>), (<*>) )
import Control.DeepSeq( ($!!) )
import Data.Monoid( (<>) )
import System.Directory( getDirectoryContents
                       , doesDirectoryExist
                       )
import System.Environment( lookupEnv )
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
import qualified Data.Text as T

import Graphics.Text.TrueType.FontType
import Graphics.Text.TrueType.Header
import Graphics.Text.TrueType.Name

catchAny :: IO a -> (E.SomeException -> IO a) -> IO a
catchAny = E.catch

loadParseFontsConf :: IO [FilePath]
loadParseFontsConf = runX (
        readDocument [withValidate no, withSubstDTDEntities no]
                     "/etc/fonts/fonts.conf"
            >>> multi (isElem >>> hasName "dir" >>> getChildren >>> getText))
 

loadUnixFontFolderList :: IO [FilePath]
loadUnixFontFolderList =
   catchAny (do conf <- loadParseFontsConf
                return $!! (</> "truetype") <$> conf)
            (const $ return [])

loadWindowsFontFolderList :: IO [FilePath]
loadWindowsFontFolderList = toFontFolder <$> lookupEnv "Windir"
  where toFontFolder (Just a) = [a </> "Fonts"]
        toFontFolder Nothing = []

fontFolders :: IO [FilePath]
fontFolders = (<>) <$> loadUnixFontFolderList 
                   <*> loadWindowsFontFolderList

findFont :: (FilePath -> IO (Maybe Font)) -> String -> FontStyle
         -> IO (Maybe FilePath)
findFont loader fontName fontStyle = do
    folders <- fontFolders 
    searchIn [("", v) | v <- folders]
  where
    fontNameText = T.pack fontName
    isMatching n (Font { _fontHeader = Just hdr
                       , _fontNames = Just names})
      | _fHdrMacStyle hdr == fontStyle &&
          fontFamilyName names == fontNameText = Just n
    isMatching _ _ = Nothing

    searchIn [] = return Nothing
    searchIn ((".", _):rest) = searchIn rest
    searchIn (("..", _):rest) = searchIn rest
    searchIn ((_, n):rest) = do
      isDirectory <- doesDirectoryExist n

      let findOrRest Nothing = searchIn rest
          findOrRest l = return l

      if isDirectory then do
        sub <- getDirectoryContents n 
        subRez <- searchIn [(s, n </> s) | s <- sub]
        findOrRest subRez
      else do
        font <- loader n
        findOrRest $ font >>= isMatching n

