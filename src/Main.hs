import Control.Exception
import Control.Monad (unless)
import Data.Binary.Put
import Data.Binary.Strict.Get
import Data.Word
import Network.Socket hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL


type Header = (Word16, B.ByteString)

main :: IO ()
main = do
  args <- getArgs
  print $ show args
  putStrLn "DNSdesu starting..."
  runServer 1053

runServer :: PortNumber -> IO ()
runServer port = withSocketsDo $ do
  sock <- socket AF_INET Datagram 0
  bindSocket sock (SockAddrInet port iNADDR_ANY)
  handler sock

handler :: Socket -> IO ()
handler conn = do
  (request, d) <- recvFrom conn 512
  let header = runGet readHeader request
  print $ show header
  let response = toStrict $ runPut makeResponse
  printWithMessage "sending: " response
  unless (B.null request) $ sendTo conn response d >> handler conn

readHeader :: Get Header
readHeader = do
  id <- getWord16be
  flags <- getByteString 2
  return (id, flags)

makeResponse :: Put
makeResponse = do
  putWord16be 1053

printWithMessage :: String -> B.ByteString -> IO ()
printWithMessage message string = do
  putStr message
  print $ show string

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks
