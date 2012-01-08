import Geography
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Network
import System.Environment ( getArgs )
import Control.Monad ( liftM )

data Command = Say String | Travel Direction deriving (Show, Read, Eq)

forever :: IO a -> IO ()
forever a = loop where loop = a >> loop

rooms :: IO String
rooms = readFile "rooms.txt"

main :: IO ()
main = do rs <- rooms
          case (roomsFromString rs) of
               Left e -> putStrLn e
               Right (r, w) -> withSocketsDo $ server r w

serverSocket :: IO Socket
serverSocket = do (portNumArg:_) <- getArgs
                  let portNum = fromIntegral (read portNumArg)
                  listenOn (PortNumber portNum)

getClientLn :: Handle -> IO String
getClientLn = liftM chompCR . hGetLine
                where chompCR "\r" = ""
                      chompCR "" = ""
                      chompCR (a:as) = a:chompCR as 

server :: Room -> World -> IO ()
server r w = do sock <- serverSocket
                forever $ do (h,_,_) <- accept sock
                             forkIO $ client h r w



client :: Handle -> Room -> World -> IO ()
client h r w = hSetBuffering h LineBuffering >> client' r
               where client' r = do hPutStrLn h (description r)
                                    clientIn <- getClientLn h
                                    case parseCommand clientIn of
                                         Just (Travel d) -> case go r d w of
                                                        Just r' -> client' r'
                                                        Nothing -> hPutStrLn h "Can't go that way" >> client' r  
                                         Just (Say s) -> sendMessage s >> client' r
                                         Nothing -> hPutStrLn h "Invalid command." >> client' r

sendMessage :: String -> IO ()
sendMessage = const $ return ()

parseCommand :: String -> Maybe Command
parseCommand "n" = Just $ Travel North
parseCommand "s" = Just $ Travel South
parseCommand "w" = Just $ Travel West
parseCommand "e" = Just $ Travel East
parseCommand "u" = Just $ Travel Up
parseCommand "d" = Just $ Travel Down
parseCommand "nw" = Just $ Travel NorthWest
parseCommand "ne" = Just $ Travel NorthEast
parseCommand "sw" = Just $ Travel SouthWest
parseCommand "se" = Just $ Travel SouthEast
parseCommand ('s':'a':'y':' ':xs) = Just $ Say xs
parseCommand _ = Nothing

