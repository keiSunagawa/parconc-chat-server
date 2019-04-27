module Chat where

import qualified Data.Map as Map
import Control.Concurrent.STM
import GHC.IO.Handle
import System.IO
import Control.Exception (finally, mask)
import Control.Monad
import Control.Concurrent.Async(race)
import System.IO
import GHC.IO.Handle

-- client statee
type ClientHandle = Handle
initialize :: ClientHandle -> IO ()
initialize handle = do
  hSetNewlineMode handle universalNewlineMode
  hSetBuffering handle LineBuffering
printMsg :: ClientHandle -> String -> IO ()
printMsg = hPutStrLn
-- case raw Socket
-- printMsg c s = send c (C8.pack $ s ++ "\n") *> return ()

getMsg :: ClientHandle -> IO String
getMsg msg = hGetLine msg
-- case raw Socket(unuse Buffering)
-- getMsg c = removeEOL . C8.unpack <$> recv c msgLimit

removeEOL :: String -> String
removeEOL xs = filter (\s -> not (s == '\n' || s =='\r'))  xs

nameLimit = 128 :: Int
msgLimit = 2048 :: Int

type ClientName = String
data Client = Client
  { clientName :: ClientName
  , clientHandle :: ClientHandle
  , clientKicked :: TVar (Maybe String)
  , clientSendChan :: TChan Message
  }

data Message = Notice String
             | Tell ClientName String
             | BrodcastMsg ClientName String
             | Command String

newClient :: ClientName -> ClientHandle -> STM Client
newClient name handle = do
  chan <- newTChan
  kicked <- newTVar Nothing
  return Client { clientName = name
                , clientHandle = handle
                , clientSendChan = chan
                , clientKicked = kicked
                }

sendMessage :: Client -> Message -> STM ()
sendMessage Client { clientSendChan=chan } msg =
  writeTChan chan msg

-- server state
data Server = Server { clients :: TVar (Map.Map ClientName Client) }

newServer :: IO Server
newServer = do
  cs <- newTVarIO Map.empty
  return Server { clients = cs }

broadcast :: Server -> Message -> STM ()
broadcast Server { clients=cs } msg = do
  clients <- readTVar cs
  mapM_ (\client -> sendMessage client msg) (Map.elems clients)

checkAddClient :: Server -> ClientName -> ClientHandle -> IO (Maybe Client)
checkAddClient server@Server{ clients=cs } name handle = atomically $ do
  clients <- readTVar cs
  if Map.member name clients
    then return Nothing
    else do client <- newClient name handle
            writeTVar cs $ Map.insert name client clients
            broadcast server $ Notice (name ++ " has connected.")
            return $ Just client

removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{ clients=cs } name = atomically $ do
  modifyTVar' cs $ Map.delete name
  broadcast server $ Notice (name ++ " has disconnect.")

talk :: ClientHandle -> Server -> IO ()
talk handle server@Server { clients=cs } = do
  initialize handle
  readName
 where
  readName = do
    printMsg handle "What is your name?"
    name <- getMsg handle
    if null name
      then readName
      else mask $ \restore -> do
         ok <- checkAddClient server name handle
         case ok of
           Nothing -> restore $ do
             let msg = "The name" ++ name ++ " is in use, please cheose another."
             printMsg handle msg
             readName
           Just client ->
             restore (runClient server client)
               `finally` removeClient server name

runClient :: Server -> Client -> IO ()
runClient server@Server {clients=cs} client@Client {clientSendChan=chan, clientKicked=kicked, clientHandle=handle} = do
  race fromServer fromReceive
  return ()
 where
   fromReceive = forever $ do
     msg <- getMsg handle
     atomically $ sendMessage client (Command msg)
   fromServer = join $ atomically $ do
     k <- readTVar kicked
     case k of
       Just reason -> return $
         printMsg handle ("You have been kicked: " ++ reason)
       Nothing -> do
         msg <- readTChan chan
         return $ do
           continue <- handleMessage server client msg
           when continue $ fromServer

handleMessage :: Server -> Client -> Message -> IO Bool
handleMessage server client@Client {clientName=cname, clientHandle=handle} msg =
  case msg of
    Notice m -> output $ "*** " ++ m
    Tell name m -> output $ "*" ++ name ++ "*: " ++ m
    BrodcastMsg name m -> output $ "<" ++ name ++ ">: " ++ m
    Command m ->
      case words m of
        ["/kick", who] -> do
          res <- atomically $ kick server who cname
          case res of
            Left err ->
              printMsg handle err
            Right () ->
              return ()
          return True
        "/tell" : who : what -> do
          res <- atomically $ tell server client who (unwords what)
          case res of
            Left err ->
              printMsg handle err
            Right () ->
              return ()
          return True
        ["/quit"] ->
          return False
        ('/':_):_ -> do
          printMsg handle ("Unrecognized Command" ++ m)
          return True
        _ ->  do
          atomically $ broadcast server $ BrodcastMsg cname m
          return True
 where
   output s = do printMsg handle s; return True

kick :: Server -> ClientName -> ClientName -> STM (Either String ())
kick Server{clients=cs} victims from = do
  clients <- readTVar cs
  case (Map.lookup victims clients) of
    Just Client{clientKicked=kickTo} ->
      Right <$> writeTVar kickTo (Just $ "kicked by " ++ from)
    Nothing ->
     return $ Left $ "user not found. name: " ++ victims


tell :: Server -> Client -> ClientName -> String -> STM (Either String ())
tell Server{clients=cs} Client{clientName=fromName} toName msg = do
  clients <- readTVar cs
  case (Map.lookup toName clients) of
    Just to' ->
      Right <$> sendMessage to' (Tell fromName msg)
    Nothing ->
      return $ Left $ "user not found. name: " ++ toName
