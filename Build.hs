#!/usr/bin/env stack
-- stack --install-ghc runghc --package shake

{-# LANGUAGE ViewPatterns #-}

import           Control.Monad
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
      removeFilesAfter "." (map (-<.> "pdf") allSrc)
      removeFilesAfter "." (map (-<.> "html") allSrc)
      removeFilesAfter ".shake" ["//*"]

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
          conf = sd </> ".revealjs.yaml"
      conf <- (conf <$) . guard <$> doesFileExist conf
      need $ src : maybeToList conf
      cmd (Cwd sd)
          "pandoc" "-t revealjs"
                   "-o " (takeFileName f)
                   "--standalone"
                   sf
                   (maybe "" takeFileName conf)


