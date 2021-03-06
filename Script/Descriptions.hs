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
import           Control.Monad
import           Data.Aeson
import           Data.Bifunctor
import           Data.Functor.Foldable
import           Data.List
import           Data.Maybe
import           GHC.Generics
import           System.FilePath
import           Text.Pandoc
import qualified Data.Text             as T

data DescrT d = Descr { descrPath  :: FilePath
                      , descrTitle :: T.Text
                      , descrDesc  :: Maybe T.Text
                      , descrNode  :: DescrNode d
                      }
  deriving (Show, Functor)

data DescrNode d = DescrFiles [DescrLink]
                 | DescrDir   [d]
  deriving (Show, Functor)

data DescrLink = DL { dlText :: T.Text, dlPath :: FilePath }
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
                   , secTitle :: T.Text
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
          descr = maybe [] parseDescr (T.unpack <$> descrDesc)
          newSec = Sec 1 descrTitle descrPath links descr
          oldSecs = case descrNode of
            DescrFiles _  -> []
            DescrDir  ss  -> (concatMap . map) (bumpSection descrPath) ss
      in  newSec : oldSecs
  where
    parseDescr :: String -> [Block]
    parseDescr d = case runPure (readMarkdown def (T.pack d)) of
        Right (Pandoc _ bs) -> bs
        Left e              -> [Para [Str (T.pack (show e))]]
    bumpSection :: FilePath -> Section -> Section
    bumpSection fp s@Sec{..} = s
      { secLevel = secLevel + 1
      , secPath  = normalise $ fp </> secPath
      }

(<://>) :: FilePath -> FilePath -> FilePath
dir <://> fp | isHttp    = fp
             | otherwise = dir </> fp
  where
    isHttp = ("http://" `isPrefixOf` fp)
          || ("https://" `isPrefixOf` fp)


toBlocks :: FilePath -> Maybe DescrLink -> Section -> [Block]
toBlocks baseURL upLink Sec{..} =
    concat [ [Header secLevel nullAttr [title]]
           , maybeToList upLinkBlock
           , links : secBody
           ]
  where
    upLinkBlock = do
      guard (secLevel == 1)
      DL{..} <- upLink
      return $ Para [Emph [Link nullAttr [Str "(up)"] (T.pack $ baseURL </> dlPath, dlText)]]
    links = BulletList . flip map secLinks $ \DL{..} ->
        [Plain [Link nullAttr [Str dlText] (T.pack $ (baseURL </> secPath) <://> dlPath, dlText)]]
    title | secLevel == 1 = Str secTitle
          | otherwise     = Link nullAttr [Str secTitle]
                              (T.pack $ baseURL </> secPath, secTitle)

descrPandoc :: FilePath -> Maybe DescrLink -> Descr -> Pandoc
descrPandoc baseURL upLink =
    Pandoc mempty . concatMap (toBlocks baseURL upLink) . flattenDescr

descrPandocs :: FilePath -> Descr -> [(FilePath, Pandoc)]
descrPandocs baseURL = (map . second) (uncurry (descrPandoc baseURL)) . unfoldDescr

unfoldDescr :: Descr -> [(FilePath, (Maybe DescrLink, Descr))]
unfoldDescr = para $ \case
    d@Descr{..} ->
      let newDescr  = d { descrNode = case descrNode of
                            DescrFiles fp -> DescrFiles fp
                            DescrDir  ds  -> DescrDir   (fst <$> ds)
                        }
          oldDescrs = case descrNode of
            DescrFiles _  -> []
            DescrDir   ds ->
              concatMap (map (bumpSub descrTitle descrPath) . snd) ds
      in  (descrPath, (Nothing, Fix newDescr)) : oldDescrs
  where
    bumpSub
        :: T.Text
        -> FilePath
        -> (FilePath, (Maybe DescrLink, Descr))
        -> (FilePath, (Maybe DescrLink, Descr))
    bumpSub addTitle addPath (oldPath, (upLink, Fix d)) = (newPath, (upLink', Fix d'))
      where
        d' = d { descrPath = normalise $ addPath </> descrPath d }
        newPath = normalise $ addPath </> oldPath
        upLink' = Just $ case upLink of
          Just dl -> dl { dlPath = normalise (addPath </> dlPath dl) }
          Nothing -> DL addTitle addPath
