#!/usr/bin/env stack
-- stack --install-ghc runghc --package shake --package pandoc --package aeson --package yaml

{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}
{-# OPTIONS_GHC -Wall         #-}

import           Control.Monad
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Data.Yaml
import           Development.Shake
import           Development.Shake.FilePath
import           Script.Descriptions
import           Text.Pandoc
import qualified Data.Map                   as M

opts :: ShakeOptions
opts = shakeOptions { shakeFiles     = ".shake"
                    , shakeVersion   = "1.0"
                    , shakeVerbosity = Normal
                    , shakeThreads   = 0
                    }

isReadme :: FilePath -> Bool
isReadme = ("README" `isInfixOf`)

validSrc :: FilePath -> Bool
validSrc src = not ("README"    `isInfixOf` src)
            && not ("reveal.js" `isInfixOf` src)

pagesBase :: FilePath
pagesBase = "http://talks.jle.im"

main :: IO ()
main = do
    readmes <- M.fromList . maybe [] (descrPandocs pagesBase)
                <$> decodeFile "descriptions.yaml"
    let readmeFiles = map (normalise . (</> "README.md")) $ M.keys readmes
    allSrc  <- filter validSrc <$> getDirectoryFilesIO "" ["//*.md"]
    shakeArgs opts $ do
      want ["all"]

      "all" ~>
        need ["beamer","reveal","readmes"]

      "beamer" ~>
        need (map (-<.> "pdf") allSrc)

      "reveal" ~>
        need (map (-<.> "html") allSrc)

      "readmes" ~>
        need readmeFiles

      "//*.pdf" %> \f -> do
        let src = f -<.> "md"
            (sd, sf) = splitFileName src
        (updirs, confs) <- getConfigs sd ".beamer.yaml"
        need $ src : confs
        cmd (Cwd sd)
            "pandoc" "-t beamer"
                     "-o " (takeFileName f)
                     "--standalone"
                     sf
                     (unwords ((updirs </>) <$> confs))

      "//*.html" %> \f -> do
        let src = f -<.> "md"
            (sd, sf) = splitFileName src
            reveal   = sd </> "reveal.js/.git"
        (updirs, confs) <- getConfigs sd ".revealjs.yaml"
        need $ src : reveal : confs
        cmd (Cwd sd)
            "pandoc" "-t revealjs"
                     "-o " (takeFileName f)
                     "--standalone"
                     sf
                     (unwords ((updirs </>) <$> confs))

      "//*/reveal.js/.git" %> \f -> do
        liftIO $ removeFiles "." [takeDirectory f]
        cmd "git" "submodule add"
                  "https://github.com/hakimel/reveal.js/"
                  (takeDirectory f)

      ["//README.md", "README.md"] |%> \f -> do
        liftIO $ putStrLn f
        need ["descriptions.yaml"]
        case M.lookup (takeDirectory f) readmes of
          Just pd -> writeFile' f (writeMarkdown def pd)
          Nothing -> error $ f ++ " not found in descriptions.yaml"

      "clean" ~> do
        removeFilesAfter ".shake" ["//*"]
        need ["clean-renders","clean-readmes"]

      "clean-renders" ~> do
        removeFilesAfter "."    (map (-<.> "pdf" ) allSrc)
        removeFilesAfter "."    (map (-<.> "html") allSrc)
        let revealDirs = map (\f -> takeDirectory f </> "reveal.js") allSrc
        traverse_ @_ @_ @_ @() (cmd "git" "rm" "--ignore-unmatch" "-r" "--cached") revealDirs
        removeFilesAfter ".git/modules" ((++ "//") <$> revealDirs)
        removeFilesAfter "." ((++ "//") <$> revealDirs)

      "clean-readmes" ~> do
        removeFilesAfter "."    readmeFiles



getConfigs :: FilePath -> String -> Action (FilePath, [FilePath])
getConfigs (splitPath -> dirs) fn =
    fmap ((updirs,) . catMaybes) . forM confs $ \c -> do
      (c <$) . guard <$> doesFileExist c
  where
    updirs = joinPath (".." <$ dirs)
    confs = map (joinPath . (++ [fn]))
          . reverse
          $ inits dirs
