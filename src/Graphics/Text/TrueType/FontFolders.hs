{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
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
    ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative             ((<$>), (<*>))
#endif

#if !MIN_VERSION_base(4,6,0)
import           Control.Exception               (tryJust)
import           Control.Monad                   (guard)
import           System.Environment              (getEnv)
import           System.IO.Error                 (isDoesNotExistError)
#else
import           System.Environment              (lookupEnv)
#endif

import           Control.Monad                   (replicateM, when)
import           Data.Binary                     (Binary (..))
import           Data.Binary.Get                 (Get, getByteString,
                                                  getWord32be)
import           Data.Binary.Put                 (Put, putByteString,
                                                  putWord32be)
import qualified Data.ByteString                 as B
import qualified Data.Map.Strict                 as M
import           System.Directory                (doesDirectoryExist,
                                                  doesFileExist,
                                                  getDirectoryContents,
                                                  getHomeDirectory)
import           System.FilePath                 ((</>))

-- import           Text.XML.HXT.Core               (getChildren, getText, hasName,
--                                                   isElem, multi, no,
--                                                   readDocument, runX,
--                                                   withSubstDTDEntities,
--                                                   withValidate, (>>>))

import qualified Text.XML.Light                  as X

import qualified Control.Exception               as E
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T

import           Control.DeepSeq                 (($!!))
import           Debug.Trace

import           Graphics.Text.TrueType.FontType
import           Graphics.Text.TrueType.Header
import           Graphics.Text.TrueType.Name

import           Data.Foldable                   (toList)
import           Data.Function                   ((&))

import           Text.PrettyPrint.ANSI.Leijen    (Pretty (pretty))
import qualified Text.PrettyPrint.ANSI.Leijen    as PP

import           Data.List                       (intersperse)
import           Data.Monoid

import           System.IO.Unsafe

intercalate :: (Monoid a) => a -> [a] -> a
intercalate e xs = mconcat $ intersperse e xs

instance Pretty X.QName where
  pretty (X.QName n _ _) = PP.string n

instance Pretty X.Attr where
  pretty (X.Attr k v) = PP.cyan (pretty k) <> "=" <> PP.red (PP.dquotes (PP.string v))

instance Pretty X.CData where
  pretty = go
    where
      go (X.CData X.CDataText     s _) = textPP s -- should escape though
      go (X.CData X.CDataVerbatim s _) = "<![CDATA[" <> cdataPP s <> "]]>"
      go (X.CData X.CDataRaw      s _) = rawPP s
      textPP  = PP.string
      cdataPP = PP.string
      rawPP   = PP.string

instance Pretty X.Content where
  pretty (X.Elem el)  = pretty el
  pretty (X.Text cd)  = pretty cd
  pretty (X.CRef str) = PP.onyellow $ PP.string str

instance Pretty X.Element where
  pretty (X.Element name attrs contents _) = PP.flatAlt multiLine oneLine
    where
      oneLine = openTagPP <> contentsPP <> closeTagPP
      multiLine = openTagPP <> PP.hardline <> contentsPP <> PP.hardline <> closeTagPP
      attrsPP = mconcat $ map ((PP.space <>) . pretty) attrs
      openTagPP  = PP.blue $ PP.langle <> pretty name <> attrsPP <> PP.rangle
      closeTagPP = PP.blue $ PP.langle <> "/" <> pretty name <> PP.rangle
      contentsPP = let cs = map pretty contents
                   in PP.indent 1 $ PP.flatAlt (PP.vcat cs) (PP.sep cs)

catchAny :: IO a -> (E.SomeException -> IO a) -> IO a
catchAny = E.catch

prettyPrint :: (Pretty p) => p -> IO ()
prettyPrint = PP.putDoc . pretty

prettyTrace :: (Pretty p) => p -> p
prettyTrace x = unsafePerformIO (prettyPrint x >> return x)

loadParseFontsConf :: IO [FilePath]
loadParseFontsConf = getPaths . T.pack . munchWS . T.unpack . T.map replaceWS
                     <$> T.readFile "/etc/fonts/fonts.conf"
  where
    munchWS (' ':' ':xs) = munchWS (' ':xs)
    munchWS (x:xs)       = x : munchWS xs
    munchWS []           = []
    replaceWS '\n' = ' '
    replaceWS '\t' = ' '
    replaceWS x    = x
    getPaths :: T.Text -> [FilePath]
    getPaths s = X.parseXML s
                 & X.onlyElems
                 & concatMap X.elChildren
                 & filter isDir
                 & map X.strContent
                 & map traceShowId
    isDir :: X.Element -> Bool
    isDir = (== "dir") . X.qName . X.elName

-- loadParseFontsConf :: IO [FilePath]
-- loadParseFontsConf = runX (readDoc "/etc/fonts/fonts.conf"
--                            >>> multi (isElem
--                                       >>> hasName "dir"
--                                       >>> getChildren
--                                       >>> getText))
--   where
--     readDoc = readDocument [withValidate no, withSubstDTDEntities no]

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
                             return $!! (</> "truetype") <$> conf)
                         (const $ return [])
    -- Quick hack, need to change XML parser to a lighter one
    --return ["/usr/share/fonts", "/usr/local/share/fonts", "~/.fonts"]

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
    , _descriptorStyle      :: !FontStyle
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

