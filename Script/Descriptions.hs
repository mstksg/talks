{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall             #-}

module Script.Descriptions (descrPandocs, Descr) where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.Functor.Foldable
import           Data.List
import           GHC.Generics
import           System.FilePath
import           Text.Pandoc

data DescrT d = Descr { descrPath  :: FilePath
                      , descrTitle :: String
                      , descrDesc  :: Maybe String
                      , descrNode  :: DescrNode d
                      }
  deriving (Show, Functor)

data DescrNode d = DescrFiles [DescrLink]
                 | DescrDir   [d]
  deriving (Show, Functor)

data DescrLink = DL { dlText :: String, dlPath :: FilePath }
  deriving (Show, Generic)

instance FromJSON d => FromJSON (DescrT d) where
    parseJSON = withObject "DescrT" $ \v ->
      Descr <$> v .:  "dir"
            <*> v .:  "title"
            <*> v .:? "description"
            <*> parseJSON (Object v)

instance FromJSON d => FromJSON (DescrNode d) where
    parseJSON = withObject "DescrNode" $ \v ->
      let parseFile = DescrFiles <$> v .: "links"
          parseDir  = DescrDir   <$> v .: "subdirs"
      in  parseFile <|> parseDir

instance FromJSON DescrLink where
    parseJSON = genericParseJSON defaultOptions
                  { fieldLabelModifier = camelTo2 '-' . drop 2 }

instance FromJSON Descr where
    parseJSON = fmap Fix . parseJSON

type Descr = Fix DescrT

data Section = Sec { secLevel :: Int
                   , secTitle :: String
                   , secPath  :: FilePath
                   , secLinks :: [DescrLink]
                   , secBody  :: [Block]
                   }
    deriving Show

flattenDescr :: Descr -> [Section]
flattenDescr = cata $ \case
    Descr{..} ->
      let links = case descrNode of
                    DescrFiles fs -> fs
                    DescrDir   _  -> []
          descr = maybe [] parseDescr descrDesc
          newSec = Sec 1 descrTitle descrPath links descr
          oldSecs = case descrNode of
            DescrFiles _  -> []
            DescrDir  ss  -> (concatMap . map) (bumpSection descrPath) ss
      in  newSec : oldSecs
  where
    parseDescr :: String -> [Block]
    parseDescr d = case readMarkdown def d of
        Right (Pandoc _ bs) -> bs
        Left e              -> [Para [Str (show e)]]
    bumpSection :: FilePath -> Section -> Section
    bumpSection fp s@Sec{..} = s { secLevel = secLevel + 1
      , secPath  = normalise $ fp </> secPath
      , secLinks = map (\dl -> dl { dlPath = fp <://> dlPath dl }) secLinks
      }

(<://>) :: FilePath -> FilePath -> FilePath
dir <://> fp | isHttp    = fp
             | otherwise = dir </> fp
  where
    isHttp = ("http://" `isPrefixOf` fp)
          || ("https://" `isPrefixOf` fp)


toBlocks :: FilePath -> Section -> [Block]
toBlocks baseURL Sec{..} = Header secLevel nullAttr [title] : links : secBody
  where
    links = BulletList . flip map secLinks $ \DL{..} ->
        [Plain [Link nullAttr [Str dlText] (baseURL <://> dlPath, dlText)]]
    title | secLevel == 1 = Str secTitle
          | otherwise     = Link nullAttr [Str secTitle]
                              (baseURL </> secPath, secTitle)

descrPandoc :: FilePath -> Descr -> Pandoc
descrPandoc baseURL =
    Pandoc mempty . concatMap (toBlocks baseURL) . flattenDescr

descrPandocs :: FilePath -> Descr -> [(FilePath, Pandoc)]
descrPandocs baseURL = (map . second) (descrPandoc baseURL) . unfoldDescr

unfoldDescr :: Descr -> [(FilePath, Descr)]
unfoldDescr = para $ \case
    d@Descr{..} ->
      let newDescr  = d { descrNode = case descrNode of
                            DescrFiles fp -> DescrFiles fp
                            DescrDir  ds  -> DescrDir   (fst <$> ds)
                        }
          oldDescrs = case descrNode of
            DescrFiles _  -> []
            DescrDir   ds ->
              concatMap ((map . first) (normalise . (descrPath </>)) . snd)
                ds
      in  (descrPath, Fix newDescr) : oldDescrs

