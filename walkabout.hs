import Geography
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Chan
import Network
import System.Environment ( getArgs )
import Control.Monad ( liftM, mapM )
import qualified Data.Map as M
import Data.IORef

data Command = Say String | Travel Direction | Hear String deriving (Show, Read, Eq)

data Client = Client Int (Chan (Maybe Command))

type Members = MVar [Client]

forever :: IO a -> IO ()
forever a = loop where loop = a >> loop

rooms :: IO String
rooms = readFile "rooms.txt"

main :: IO ()
main = do rs <- rooms
          case (roomsFromString rs) of
               Left e -> putStrLn e
               Right w -> withSocketsDo $ do { w' <- ioed w; xserver w' } 
          where xserver w = case M.lookup (RoomId 0) w of 
                                 (Just r) -> server r w
                ioed w = do iolist <- (mapM g $ M.toList w)
                            return $ M.fromList iolist
                g (k, v) = do r <- newMVar []
                              return (k, v { extra = Just r })
                 

serverSocket :: IO Socket
serverSocket = do (portNumArg:_) <- getArgs
                  let portNum = fromIntegral (read portNumArg)
                  listenOn (PortNumber portNum)

getClientLn :: Handle -> IO String
getClientLn = liftM chompCR . hGetLine
                where chompCR "\r" = ""
                      chompCR "" = ""
                      chompCR (a:as) = a:chompCR as 

-- server :: Room a -> World a -> IO ()
server r w = do sock <- serverSocket
                counter <- newIORef 0
                forever $ do (h,_,_) <- accept sock
                             count <- readIORef counter
                             chan <- newChan
                             writeIORef counter (count + 1)
                             forkIO $ client (Client count chan) h r w



getUserCommand :: Handle -> IO (Maybe Command)
getUserCommand h = getClientLn h >>= return . parseCommand

pushUserCommands (Client _ c) h = forever $ do cmd <- getUserCommand h
                                               writeChan c cmd


getCommand c = readChan c


enter c (Room _ _ Nothing)  = return ()
enter (Client i _) (Room _ _ (Just v)) = do r <- takeMVar v
                                            putMVar v (filter (\(Client j _) -> i /= j) r)

leave c (Room _ _ Nothing)  = return ()
leave c (Room _ _ (Just v)) = do r <- takeMVar v
                                 putMVar v (c:r)
 

client :: Client -> Handle -> Room (MVar [Client]) -> World (MVar [Client])  -> IO ()
client c h r w = hSetBuffering h LineBuffering >> client' r
                 where client' r = do hPutStrLn h (description r)
                                      let (Client _ ch) = c
                                      forkIO $ pushUserCommands c h
                                      cmd <- getCommand ch
                                      case cmd of
                                           Just (Travel d) -> case go r d w of
                                                                   Just r' -> leave c r >> enter c r' >> client' r'
                                                                   Nothing -> hPutStrLn h "Can't go that way" >> client' r  
                                           Just (Say s) -> sendMessage s >> client' r
                                           Just (Hear s) -> hPutStrLn h s >> client' r
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


