{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Control.Monad (unless)
import Data.Binary.Put
import Data.Binary.Strict.Get
import Data.Word
import Network.Socket hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import System.Environment
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC (putStrLn)


main :: IO ()
main = do
  args <- getArgs
  print $ show args
  BC.putStrLn "DNSdesu starting..."
  runServer 1053

runServer :: PortNumber -> IO ()
runServer port = withSocketsDo $ do
  sock <- socket AF_INET Datagram 0
  bindSocket sock (SockAddrInet port iNADDR_ANY)
  handler sock

handler :: Socket -> IO ()
handler conn = do
  (request, d) <- recvFrom conn 512
  let id = runGet readHeader request
  print $ show id
  let response = toStrict $ runPut makeResponse
  unless (B.null request) $ sendTo conn response d >> handler conn

readHeader :: Get (Word16)
readHeader = do
  id <- getWord16be
  return id

makeResponse :: Put
makeResponse = do
  putWord16be 1053

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks
