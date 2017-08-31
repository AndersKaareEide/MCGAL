{-# LANGUAGE CPP #-}
module Paths (getStaticDir) where

import Control.Monad
import System.FilePath

#if defined(CABAL)
-- using cabal
import qualified Paths_Masterprosjekt (getDataDir)

getStaticDir :: IO FilePath
getStaticDir = do
  path <- (</> "static") `liftM` Paths_Masterprosjekt.getDataDir
  putStr show path


#elif defined(FPCOMPLETE)
-- Used by stack runhaskell apparently
getStaticDir :: IO FilePath
getStaticDir = "static"

#else
import qualified Paths_Masterprosjekt
-- using GHCi
-- Also used when running the executable it would seem
getStaticDir :: IO FilePath
getStaticDir = (</> "src/static") `liftM` Paths_Masterprosjekt.getDataDir

#endif
