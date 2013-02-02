-----------------------------------------------------------------------------
-- |
-- Module      : Hoodle.Util.Process
-- Copyright   : (c) 2013 Ian-Woo Kim
--
-- License     : BSD3
-- Maintainer  : Ian-Woo Kim <ianwookim@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-----------------------------------------------------------------------------

module Hoodle.Util.Process where

import Control.Concurrent
import Control.Monad
import Control.Monad.Loops 
import qualified Data.ByteString.Lazy as B
-- import Data.Monoid
-- import Data.UUID
import Data.UUID.V4 
import System.Directory
import System.FilePath
import System.IO
-- import System.IO.MMap
import System.Posix.Files
-- import System.Posix.Files.ByteString 
import System.Posix.IO
import System.Posix.Process 

pipeReceiver :: FilePath -> (B.ByteString -> IO a) -> IO a
pipeReceiver fp receiver = do 
  pipestr <- iterateUntil (not.B.null) $ threadDelay 10000 >> B.readFile fp 
  receiver pipestr


mkTmpFileName :: IO FilePath 
mkTmpFileName = do
  tdir <- getTemporaryDirectory 
  tuuid <- nextRandom
  return $ tdir </> show tuuid <.> "fifo"

existThenRemove :: FilePath -> IO () 
existThenRemove fp = fileExist fp >>= \b -> when b (removeLink fp) 

pipeAction :: IO () -> (B.ByteString -> IO ()) -> IO () 
pipeAction sender receiver = do 
  filename <- mkTmpFileName 
  existThenRemove filename 
  createNamedPipe filename (unionFileModes ownerReadMode ownerWriteMode)
  forkProcess $ do  
    fd <- openFd filename WriteOnly Nothing defaultFileFlags
    dupTo fd stdOutput 
    closeFd fd
    sender 
    hFlush stdout 
  pipeReceiver filename receiver 
  removeLink filename  
