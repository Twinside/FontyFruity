{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module Graphics.Text.TrueType.LanguageIds
    ( PlatformId( .. )
    , UnicodePlatformSpecific( .. )
    , MacPlatformId( .. )
    , MacLanguage( .. )
    , platformToWord
    , unicodePlatformSpecificToId
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>) )
#endif

import Data.Binary( Binary( .. ) )
import Data.Binary.Get( getWord16be )
import Data.Binary.Put( putWord16be )
import Data.Word( Word16 )
import qualified Data.Map.Strict as M

data PlatformId
    = PlatformUnicode   -- ^ 0
    | PlatformMacintosh -- ^ 1
    | PlatformISO       -- ^ 2
    | PlatformWindows   -- ^ 3
    | PlatformCustom    -- ^ 4
    | PlatformId Word16
    deriving (Eq, Ord, Show)

instance Binary PlatformId where
    put = putWord16be . platformToWord
    get = do
      val <- getWord16be
      return $ case val of
        0 -> PlatformUnicode
        1 -> PlatformMacintosh
        2 -> PlatformISO
        3 -> PlatformWindows
        4 -> PlatformCustom
        n -> PlatformId n

platformToWord :: PlatformId -> Word16
platformToWord = p where
  p PlatformUnicode = 0
  p PlatformMacintosh = 1
  p PlatformISO = 2
  p PlatformWindows = 3
  p PlatformCustom = 4
  p (PlatformId v) = v

data UnicodePlatformSpecific
    = UnicodePlatform1_0
    | UnicodePlatform1_1
    | UnicodeISO10645
    | UnicodeBMPOnly2_0
    | UnicodeFull2_0
    | UnicodeVariation
    | UnicodeFull
    deriving (Eq, Show)

unicodePlatformSpecificToId :: UnicodePlatformSpecific -> Word16
unicodePlatformSpecificToId = go
  where
    go UnicodePlatform1_0 = 0
    go UnicodePlatform1_1 = 1
    go UnicodeISO10645    = 2
    go UnicodeBMPOnly2_0  = 3
    go UnicodeFull2_0     = 4
    go UnicodeVariation   = 5
    go UnicodeFull        = 6

instance Binary UnicodePlatformSpecific where
    put = putWord16be . unicodePlatformSpecificToId 

    get = do
       v <- getWord16be
       return $ case v of
          0 -> UnicodePlatform1_0
          1 -> UnicodePlatform1_1
          2 -> UnicodeISO10645
          3 -> UnicodeBMPOnly2_0
          4 -> UnicodeFull2_0
          5 -> UnicodeVariation
          6 -> UnicodeFull
          _ -> UnicodeFull

data MacPlatformId
   = MacSpecificRoman
   | MacSpecificJapanese
   | MacSpecificChineseTraditional
   | MacSpecificKorean
   | MacSpecificArabic
   | MacSpecificHebrew
   | MacSpecificGreek
   | MacSpecificRussian
   | MacSpecificRSymbol
   | MacSpecificDevanagari
   | MacSpecificGurmukhi
   | MacSpecificGujarati
   | MacSpecificOriya
   | MacSpecificBengali
   | MacSpecificTamil
   | MacSpecificTelugu
   | MacSpecificKannada
   | MacSpecificMalayalam
   | MacSpecificSinhalese
   | MacSpecificBurmese
   | MacSpecificKhmer
   | MacSpecificThai
   | MacSpecificLaotian
   | MacSpecificGeorgian
   | MacSpecificArmenian
   | MacSpecificChineseSimplified
   | MacSpecificTibetan
   | MacSpecificMongolian
   | MacSpecificGeez
   | MacSpecificSlavic
   | MacSpecificVietnamese
   | MacSpecificSindhi
   | MacSpecificUninterpreted
   deriving (Eq, Ord, Show)

macSpecifcIdList :: [MacPlatformId]
macSpecifcIdList =
    [ MacSpecificRoman, MacSpecificJapanese, MacSpecificChineseTraditional
    , MacSpecificKorean, MacSpecificArabic, MacSpecificHebrew
    , MacSpecificGreek, MacSpecificRussian, MacSpecificRSymbol
    , MacSpecificDevanagari, MacSpecificGurmukhi, MacSpecificGujarati
    , MacSpecificOriya, MacSpecificBengali, MacSpecificTamil
    , MacSpecificTelugu, MacSpecificKannada, MacSpecificMalayalam
    , MacSpecificSinhalese, MacSpecificBurmese, MacSpecificKhmer
    , MacSpecificThai, MacSpecificLaotian, MacSpecificGeorgian
    , MacSpecificArmenian, MacSpecificChineseSimplified, MacSpecificTibetan
    , MacSpecificMongolian, MacSpecificGeez, MacSpecificSlavic
    , MacSpecificVietnamese, MacSpecificSindhi, MacSpecificUninterpreted
    ]


prepareSpecificMaps :: Ord a => [a] -> (M.Map a Word16, M.Map Word16 a)
prepareSpecificMaps lst = (toWord, toPlatform)
  where
    toWord = M.fromList $ zip lst [0 ..]
    toPlatform = M.fromList $ zip [0 ..] lst

mapSpecifcIdMaps :: ( M.Map MacPlatformId Word16
                    , M.Map Word16 MacPlatformId )
mapSpecifcIdMaps = prepareSpecificMaps macSpecifcIdList

instance Binary MacPlatformId where
    get = finder <$> getWord16be
      where
        (_, to) = mapSpecifcIdMaps
        finder v = M.findWithDefault MacSpecificUninterpreted v to

    put v = putWord16be val
      where
        (from, _) = mapSpecifcIdMaps
        val = M.findWithDefault 32 v from

data MacLanguage
    = MacLangEnglish
    | MacLangFrench
    | MacLangGerman
    | MacLangItalian
    | MacLangDutch
    | MacLangSwedish
    | MacLangSpanish
    | MacLangDanish
    | MacLangPortuguese
    | MacLangNorwegian
    | MacLangHebrew
    | MacLangJapanese
    | MacLangArabic
    | MacLangFinnish
    | MacLangGreek
    | MacLangInuktitut
    | MacLangIcelandic
    | MacLangMaltese
    | MacLangTurkish
    | MacLangCroatian
    | MacLangChineseTraditional
    | MacLangUrdu
    | MacLangHindi
    | MacLangThai
    | MacLangKorean
    | MacLangLithuanian
    | MacLangPolish
    | MacLangHungarian
    | MacLangEstonian
    | MacLangLatvian
    | MacLangSami
    | MacLangFaroese
    | MacLangFarsiPersian
    | MacLangRussian
    | MacLangChineseSimplified
    | MacLangFlemish
    | MacLangIrishGaelic
    | MacLangAlbanian
    | MacLangRomanian
    | MacLangCzech
    | MacLangSlovak
    | MacLangSlovenian
    | MacLangYiddish
    | MacLangSerbian
    | MacLangMacedonian
    | MacLangBulgarian
    | MacLangUkrainian
    | MacLangByelorussian
    | MacLangUzbek
    | MacLangKazakh
    | MacLangAzerbaijaniCyrillic
    | MacLangAzerbaijaniArabic
    | MacLangArmenian
    | MacLangGeorgian
    | MacLangMoldavian
    | MacLangKirghiz
    | MacLangTajiki
    | MacLangTurkmen
    | MacLangMongolian
    | MacLangMongolianCyrillic
    | MacLangPashto
    | MacLangKurdish
    | MacLangKashmiri
    | MacLangSindhi
    | MacLangTibetan
    | MacLangNepali
    | MacLangSanskrit
    | MacLangMarathi
    | MacLangBengali
    | MacLangAssamese
    | MacLangGujarati
    | MacLangPunjabi
    | MacLangOriya
    | MacLangMalayalam
    | MacLangKannada
    | MacLangTamil
    | MacLangTelugu
    | MacLangSinhalese
    | MacLangBurmese
    | MacLangKhmer
    | MacLangLao
    | MacLangVietnamese
    | MacLangIndonesian
    | MacLangTagalong
    | MacLangMalayRoman
    | MacLangMalayArabic
    | MacLangAmharic
    | MacLangTigrinya
    | MacLangGalla
    | MacLangSomali
    | MacLangSwahili
    | MacLangKinyarwandaRuanda
    | MacLangRundi
    | MacLangNyanjaChewa
    | MacLangMalagasy
    | MacLangEsperanto
    | MacLangWelsh
    | MacLangBasque
    | MacLangCatalan
    | MacLangLatin
    | MacLangQuenchua
    | MacLangGuarani
    | MacLangAymara
    | MacLangTatar
    | MacLangUighur
    | MacLangDzongkha
    | MacLangJavanese
    | MacLangSundanese
    | MacLangGalician
    | MacLangAfrikaans
    | MacLangBreton
    | MacLangScottishGaelic
    | MacLangManxGaelic
    | MacLangIrishGaelicWithDot
    | MacLangTongan
    | MacLangGreekPolytonic
    | MacLangGreenlandic
    | MacLangAzerbaijani
    deriving (Eq, Ord, Show)

macLangList :: [MacLanguage]
macLangList =
    [ MacLangEnglish , MacLangFrench , MacLangGerman , MacLangItalian
    , MacLangDutch , MacLangSwedish , MacLangSpanish , MacLangDanish
    , MacLangPortuguese , MacLangNorwegian , MacLangHebrew , MacLangJapanese
    , MacLangArabic , MacLangFinnish , MacLangGreek , MacLangInuktitut
    , MacLangIcelandic , MacLangMaltese , MacLangTurkish , MacLangCroatian
    , MacLangChineseTraditional , MacLangUrdu , MacLangHindi , MacLangThai
    , MacLangKorean , MacLangLithuanian , MacLangPolish , MacLangHungarian
    , MacLangEstonian , MacLangLatvian , MacLangSami , MacLangFaroese
    , MacLangFarsiPersian , MacLangRussian , MacLangChineseSimplified
    , MacLangFlemish
    , MacLangIrishGaelic , MacLangAlbanian , MacLangRomanian , MacLangCzech
    , MacLangSlovak , MacLangSlovenian , MacLangYiddish , MacLangSerbian
    , MacLangMacedonian , MacLangBulgarian , MacLangUkrainian , MacLangByelorussian
    , MacLangUzbek , MacLangKazakh , MacLangAzerbaijaniCyrillic
    , MacLangAzerbaijaniArabic
    , MacLangArmenian , MacLangGeorgian , MacLangMoldavian , MacLangKirghiz
    , MacLangTajiki , MacLangTurkmen , MacLangMongolian , MacLangMongolianCyrillic
    , MacLangPashto , MacLangKurdish , MacLangKashmiri , MacLangSindhi
    , MacLangTibetan , MacLangNepali , MacLangSanskrit , MacLangMarathi
    , MacLangBengali , MacLangAssamese , MacLangGujarati , MacLangPunjabi
    , MacLangOriya , MacLangMalayalam , MacLangKannada , MacLangTamil
    , MacLangTelugu , MacLangSinhalese , MacLangBurmese , MacLangKhmer
    , MacLangLao , MacLangVietnamese , MacLangIndonesian , MacLangTagalong
    , MacLangMalayRoman , MacLangMalayArabic , MacLangAmharic , MacLangTigrinya
    , MacLangGalla , MacLangSomali , MacLangSwahili , MacLangKinyarwandaRuanda
    , MacLangRundi , MacLangNyanjaChewa , MacLangMalagasy , MacLangEsperanto
    , MacLangWelsh , MacLangBasque , MacLangCatalan , MacLangLatin
    , MacLangQuenchua , MacLangGuarani , MacLangAymara , MacLangTatar
    , MacLangUighur , MacLangDzongkha , MacLangJavanese , MacLangSundanese
    , MacLangGalician , MacLangAfrikaans , MacLangBreton , MacLangScottishGaelic
    , MacLangManxGaelic , MacLangIrishGaelicWithDot , MacLangTongan
    , MacLangGreekPolytonic , MacLangGreenlandic , MacLangAzerbaijani
    ]

mapLangIdMaps :: (M.Map MacLanguage Word16, M.Map Word16 MacLanguage)
mapLangIdMaps = prepareSpecificMaps macLangList

instance Binary MacLanguage where
    get = finder <$> getWord16be
      where
        (_, to) = mapLangIdMaps
        finder v = M.findWithDefault MacLangEnglish v to

    put v = putWord16be val
      where
        (from, _) = mapLangIdMaps
        val = M.findWithDefault 0 v from

