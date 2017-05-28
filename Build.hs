#!/usr/bin/env stack
-- stack --install-ghc runghc --package shake

{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

import           Control.Monad
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Development.Shake
import           Development.Shake.FilePath

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

main :: IO ()
main = getDirectoryFilesIO "" ["//*.md"] >>=
          \(filter validSrc -> allSrc) -> shakeArgs opts $ do

    want ["all"]

    "all" ~>
      need ["beamer","reveal"]

    "beamer" ~>
      need (map (-<.> "pdf") allSrc)

    "reveal" ~>
      need (map (-<.> "html") allSrc)

    "clean" ~> do
      removeFilesAfter ".shake" ["//*"]
      removeFilesAfter "."    (map (-<.> "pdf" ) allSrc)
      removeFilesAfter "."    (map (-<.> "html") allSrc)
      let revealDirs = map (\f -> takeDirectory f </> "reveal.js") allSrc
      traverse_ @_ @_ @_ @() (cmd "git" "rm" "--ignore-unmatch" "-r" "--cached") revealDirs
      removeFilesAfter ".git/modules" ((++ "//") <$> revealDirs)
      removeFilesAfter "." ((++ "//") <$> revealDirs)


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

getConfigs :: FilePath -> String -> Action (FilePath, [FilePath])
getConfigs (splitPath -> dirs) fn =
    fmap ((updirs,) . catMaybes) . forM confs $ \c -> do
      (c <$) . guard <$> doesFileExist c
  where
    updirs = joinPath (".." <$ dirs)
    confs = map (joinPath . (++ [fn]))
          . reverse
          $ inits dirs
