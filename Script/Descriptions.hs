{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Script.Descriptions (descrPandocs, Descr) where

import           Control.Applicative
import           Data.Aeson
import           Data.Bifunctor
import           Data.Functor.Foldable
import           System.FilePath
import           Text.Pandoc
import qualified Data.Text             as T

data DescrT d = Descr { descrPath  :: FilePath
                      , descrTitle :: String
                      , descrDesc  :: Maybe String
                      , descrNode  :: DescrNode d
                      }
  deriving (Show, Functor)

data DescrNode d = DescrFile FilePath
                 | DescrDir  [d]
  deriving (Show, Functor)

instance FromJSON d => FromJSON (DescrT d) where
    parseJSON o@(Object v) =
      Descr <$> v .:  "dir"
            <*> v .:  "title"
            <*> v .:? "description"
            <*> parseJSON o

instance FromJSON d => FromJSON (DescrNode d) where
    parseJSON o@(Object v) = parseFile <|> parseDir
      where
        parseFile = DescrFile <$> v .: "link"
        parseDir  = DescrDir  <$> v .: "subdirs"

instance FromJSON Descr where
    parseJSON o = Fix <$> parseJSON o

type Descr = Fix DescrT

data Section = Sec { secLevel :: Int
                   , secTitle :: String
                   , secPath  :: FilePath
                   , secBody  :: [Block]
                   }
    deriving Show

flattenDescr :: Descr -> [Section]
flattenDescr = cata $ \case
    Descr{..} ->
      let link = case descrNode of
            DescrFile fp -> descrPath </> fp
            DescrDir _   -> descrPath
          descr = maybe [] parseDescr descrDesc
          newSec = Sec 1 descrTitle link descr
          oldSecs = case descrNode of
            DescrFile _  -> []
            DescrDir  ss -> (concatMap . map) (bumpSection descrPath) ss
      in  newSec : oldSecs
  where
    parseDescr :: String -> [Block]
    parseDescr d = case readMarkdown def d of
        Right (Pandoc _ bs) -> bs
        Left e              -> [Para [Str (show e)]]
    bumpSection :: FilePath -> Section -> Section
    bumpSection fp s@Sec{..} =
        s { secLevel = secLevel + 1
          , secPath  = fp </> secPath
          }

toBlocks :: Section -> [Block]
toBlocks Sec{..} = Header secLevel nullAttr [Str secTitle] : secBody

descrPandoc :: Descr -> Pandoc
descrPandoc = Pandoc mempty . concatMap toBlocks . flattenDescr

descrPandocs :: Descr -> [(FilePath, Pandoc)]
descrPandocs = (map . second) descrPandoc . unfoldDescr

unfoldDescr :: Descr -> [(FilePath, Descr)]
unfoldDescr = para $ \case
    d@Descr{..} ->
      let newDescr  = d { descrNode = case descrNode of
                            DescrFile fp -> DescrFile fp
                            DescrDir  ds -> DescrDir  (fst <$> ds)
                        }
          oldDescrs = case descrNode of
            DescrFile fp -> []
            DescrDir  ds ->
              concatMap ((map . first) (normalise . (descrPath </>)) . snd)
                ds
      in  (descrPath, Fix newDescr) : oldDescrs

