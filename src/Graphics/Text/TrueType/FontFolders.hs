
module Graphics.Text.TrueType.FontFolders
    ( loadUnixFontFolderList
    , loadWindowsFontFolderList
    , fontFolders
    , findFont
    , FontCache( .. )
    , FontDescriptor( .. )
    , buildFontCache
    ) where

import Control.Applicative( (<$>), (<*>) )
{-import Control.DeepSeq( ($!!) )-}
{-import Data.Monoid( (<>) )-}
import System.Directory( getDirectoryContents
                       , doesDirectoryExist
                       )
import qualified Data.Map as M
import System.Environment( lookupEnv )
import System.FilePath( (</>) )
{-
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
-- -}

{-import qualified Control.Exception as E-}
import qualified Data.Text as T

import Graphics.Text.TrueType.FontType
import Graphics.Text.TrueType.Header
import Graphics.Text.TrueType.Name

{-catchAny :: IO a -> (E.SomeException -> IO a) -> IO a-}
{-catchAny = E.catch-}

{-
loadParseFontsConf :: IO [FilePath]
loadParseFontsConf = runX (
        readDocument [withValidate no, withSubstDTDEntities no]
                     "/etc/fonts/fonts.conf"
            >>> multi (isElem >>> hasName "dir" >>> getChildren >>> getText))
 
-- -}

loadUnixFontFolderList :: IO [FilePath]
loadUnixFontFolderList =
    -- Quick hack, need to change XML parser to a lighter one
    return ["/usr/share/fonts", "/usr/local/share/fonts", "~/.fonts"]
    {-
   catchAny (do conf <- loadParseFontsConf
                return $!! (</> "truetype") <$> conf)
            (const $ return [])
            --}

loadWindowsFontFolderList :: IO [FilePath]
loadWindowsFontFolderList = toFontFolder <$> lookupEnv "Windir"
  where toFontFolder (Just a) = [a </> "Fonts"]
        toFontFolder Nothing = []

loadOsXFontFolderList :: IO [FilePath]
loadOsXFontFolderList = return
    ["~/Library/Fonts"
    ,"/Library/Fonts"
    ,"/System/Library/Fonts"
    ,"/System Folder/Fonts"
    ]
    

fontFolders :: IO [FilePath]
fontFolders = do
    unix <- loadUnixFontFolderList
    win <- loadWindowsFontFolderList
    osx <- loadOsXFontFolderList
    return $ unix ++ win ++ osx

-- | A font descriptor is a key used to find a font
-- in a font cache.
data FontDescriptor = FontDescriptor
    { -- | The family name of the font
      _descriptorFamilyName :: T.Text
      -- | The desired style
    , _descriptorStyle :: FontStyle
    }
    deriving (Eq, Ord, Show)

-- | A font cache is a cache listing all the found
-- fonts on the system, allowing faster font lookup
-- once created
newtype FontCache = 
    FontCache (M.Map FontDescriptor FilePath)

-- | Look in the system's folder for usable fonts.
buildFontCache :: (FilePath -> IO (Maybe Font)) -> IO FontCache
buildFontCache loader = do
  folders <- fontFolders 
  found <- build [("", v) | v <- folders]
  return . FontCache
         $ M.fromList [(d, path) | (Just d, path) <- found]
  where
    descriptorOf Font { _fontHeader = Just hdr
                      , _fontNames = Just names} =
        Just $ FontDescriptor (fontFamilyName names)
                              (_fHdrMacStyle hdr)
    descriptorOf _ = Nothing

    build [] = return []
    build ((".", _):rest) = build rest
    build (("..", _):rest) = build rest
    build ((_, n):rest) = do
      isDirectory <- doesDirectoryExist n

      if isDirectory then do
        sub <- getDirectoryContents n 
        (++) <$> build [(s, n </> s) | s <- sub]
             <*> build rest
      else do
        f <- loader n
        case f of
          Nothing -> build rest
          Just fo -> ((descriptorOf fo, n) :) <$> build rest

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

