import System.Environment

main :: IO ()
main = do
  args <- getArgs
  print $ show args
  putStrLn "watashi wa DNSdesu"
