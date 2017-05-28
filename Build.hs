#!/usr/bin/env stack
-- stack --install-ghc runghc --package shake

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
      liftIO $ do
        removeFiles "."    (map (-<.> "pdf" ) allSrc)
        removeFiles "."    (map (-<.> "html") allSrc)
        let revealDirs = map (\f -> takeDirectory f </> "reveal.js") allSrc
        traverse_ @_ @_ @_ @() (cmd "git" "rm" "--ignore-unmatch" "-r" "--cached") revealDirs
        removeFiles ".git/modules" ((++ "//") <$> revealDirs)
        removeFiles "." ((++ "//") <$> revealDirs)


    "//*.pdf" %> \f -> do
      let src = f -<.> "md"
          (sd, sf) = splitFileName src
          conf = sd </> ".beamer.yaml"
      conf <- (conf <$) . guard <$> doesFileExist conf
      need $ src : maybeToList conf
      cmd (Cwd sd)
          "pandoc" "-t beamer"
                   "-o " (takeFileName f)
                   "--standalone"
                   sf
                   (maybe "" takeFileName conf)

    "//*.html" %> \f -> do
      let src = f -<.> "md"
          (sd, sf) = splitFileName src
          conf     = sd </> ".revealjs.yaml"
          reveal   = sd </> "reveal.js/.git"
      conf <- (conf <$) . guard <$> doesFileExist conf
      need $ src : reveal : maybeToList conf
      cmd (Cwd sd)
          "pandoc" "-t revealjs"
                   "-o " (takeFileName f)
                   "--standalone"
                   sf
                   (maybe "" takeFileName conf)

    "//*/reveal.js/.git" %> \f -> do
      liftIO $ removeFiles "." [takeDirectory f]
      cmd "git" "submodule add"
                "https://github.com/hakimel/reveal.js/"
                (takeDirectory f)

