module Main where

import Control.Monad.STM
import Chat
import Text.Printf
import Control.Monad(forever)
import Control.Concurrent(forkFinally)
import Network.Socket
import System.IO

main :: IO ()
main = withSocketsDo $ do
  server <- newServer
  addr <- resolve port
  sock <- open addr
  printf "Listening on port %s\n" port
  forever $ do
    (csock, addr) <- accept sock
    handle <- socketToHandle csock ReadWriteMode
    putStrLn $ "Accept connection from " ++ (show addr)
    forkFinally (talk handle server) (\n -> hClose handle)
 where
   resolve port = do
     let hints = defaultHints { addrFlags = [AI_PASSIVE]
                              , addrSocketType = Stream }
     addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
     return addr
   open addr = do
     sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
     setSocketOption sock ReuseAddr 1
     -- If the prefork technique is not used,
     -- set CloseOnExec for the security reasons.
     let fd = fdSocket sock
     setCloseOnExecIfNeeded fd
     bind sock (addrAddress addr)
     listen sock 10
     return sock

   port = "7777"
