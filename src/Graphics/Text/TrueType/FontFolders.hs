{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Graphics.Text.TrueType.FontFolders
    ( loadUnixFontFolderList
    , loadWindowsFontFolderList
    , fontFolders
    , findFont
    , descriptorOf
    , FontCache( .. )
    , FontDescriptor( .. )
    , emptyFontCache
    , buildFontCache
    , enumerateFonts
    , descriptorOf
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<*>), (<$>) )
#endif

#if !MIN_VERSION_base(4,6,0)
import Control.Monad( guard )
import Control.Exception( tryJust )
import System.IO.Error( isDoesNotExistError )
import System.Environment( getEnv )
#else
import System.Environment( lookupEnv )
#endif

import Control.Monad( when, replicateM )
import System.Directory( getDirectoryContents
                       , getHomeDirectory
                       , doesDirectoryExist
                       , doesFileExist
                       )
import qualified Data.ByteString as B
import Data.Binary( Binary( .. ) )
import Data.Binary.Get( Get
                      , getWord32be
                      , getByteString
                      )
import Data.Binary.Put( Put
                      , putWord32be
                      , putByteString )
import qualified Data.Map.Strict as M
import System.FilePath( (</>) )

import Text.XML.Light( elChildren
                     , elName
                     , onlyElems
                     , parseXML
                     , qName
                     , strContent )

import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)

import Graphics.Text.TrueType.FontType
import Graphics.Text.TrueType.Header
import Graphics.Text.TrueType.Name

import Control.DeepSeq (($!!))

catchAny :: IO a -> (E.SomeException -> IO a) -> IO a
catchAny = E.catch

loadParseFontsConf :: IO [FilePath]
loadParseFontsConf = getPaths <$> T.readFile "/etc/fonts/fonts.conf"
  where
    getPaths s = map strContent
                 $ filter ((== "dir") . qName . elName)
                 $ concatMap elChildren
                 $ onlyElems
                 $ parseXML s

#if !MIN_VERSION_base(4,6,0)
lookupEnv :: String -> IO (Maybe String)
lookupEnv varName = do
  v <- tryJust (guard . isDoesNotExistError) $ getEnv varName
  case v of
    Left _ -> return Nothing
    Right val -> return $ Just val
#endif

loadUnixFontFolderList :: IO [FilePath]
loadUnixFontFolderList = catchAny
                         (do conf <- loadParseFontsConf
                             return $!! conf ++ map (</> "truetype") conf)
                         (const $ return defaults)
  where
    defaults = ["/usr/share/fonts", "/usr/local/share/fonts", "~/.fonts"]

loadWindowsFontFolderList :: IO [FilePath]
loadWindowsFontFolderList = toFontFolder <$> lookupEnv "Windir"
  where toFontFolder (Just a) = [a </> "Fonts"]
        toFontFolder Nothing = []

loadOsXFontFolderList :: IO [FilePath]
loadOsXFontFolderList = do
    home <- getHomeDirectory
    return [home </> "Library" </> "Fonts"
           ,"/" </> "Library" </> "Fonts"
           ,"/" </> "System" </> "Library" </> "Fonts"
           ,"/" </> "System Folder" </> "Fonts"
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
      _descriptorFamilyName :: !T.Text
      -- | The desired style
    , _descriptorStyle :: !FontStyle
    }
    deriving (Eq, Ord, Show)

instance Binary FontDescriptor where
  put (FontDescriptor t s) = put (T.unpack t) >> put s
  get = FontDescriptor <$> (T.pack <$> get) <*> get

-- | A font cache is a cache listing all the found
-- fonts on the system, allowing faster font lookup
-- once created
--
-- FontCache is an instance of binary, to get okish
-- performance you should save it in a file somewhere
-- instead of rebuilding it everytime!
--
-- The font cache is dependent on the version
-- of rasterific, you must rebuild it for every
-- version.
newtype FontCache =
    FontCache (M.Map FontDescriptor FilePath)
    deriving Show

-- | Font cache with no pre-existing fonts in it.
emptyFontCache :: FontCache
emptyFontCache = FontCache M.empty

signature :: B.ByteString
signature = "FontyFruity__FONTCACHE:0.5"

putFontCache :: FontCache -> Put
putFontCache (FontCache cache) = do
  putByteString signature
  putWord32be . fromIntegral $ M.size cache
  mapM_ put $ M.toList cache

getFontCache :: Get FontCache
getFontCache = do
  str <- getByteString $ B.length signature
  when (str /= signature) $
      fail "Invalid font cache"
  count <- fromIntegral <$> getWord32be
  FontCache . M.fromList <$> replicateM count get

instance Binary FontCache where
  put = putFontCache
  get = getFontCache

-- | Returns a list of descriptors of fonts stored in the given cache.
enumerateFonts :: FontCache -> [FontDescriptor]
enumerateFonts (FontCache fs) = M.keys fs

-- | If possible, returns a descriptor of the Font.
descriptorOf :: Font -> Maybe FontDescriptor
descriptorOf font = do
  hdr <- _fontHeader font
  names <- _fontNames font
  return $ FontDescriptor (fontFamilyName names) (_fHdrMacStyle hdr)

-- | Look in the system's folder for usable fonts.
buildFontCache :: (FilePath -> IO (Maybe Font)) -> IO FontCache
buildFontCache loader = do
  folders <- fontFolders
  found <- build [("", v) | v <- folders]
  return . FontCache
         $ M.fromList [(d, path) | (Just d, path) <- found
                                 , _descriptorFamilyName d /= ""]
  where
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
        isFile <- doesFileExist n
        if isFile then do
            f <- loader n
            case f of
              Nothing -> build rest
              Just fo -> ((descriptorOf fo, n) :) <$> build rest
        else build rest

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
        isFile <- doesFileExist n
        if isFile then do
          font <- loader n
          findOrRest $ font >>= isMatching n
        else searchIn rest
