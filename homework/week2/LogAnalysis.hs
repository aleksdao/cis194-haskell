{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Data.List (intercalate)

-- main = do
--   errorLogs <- readFile "error.log"
--   let result = sortMessages (parse errorLogs)
--   putStrLn $ show result

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree

getMessageType :: String -> MessageType
getMessageType "I" = Info
getMessageType "W" = Warning

parseMessage :: String -> LogMessage
parseMessage logString =
  let
    getMessageString words =
      intercalate " " words
  in
    case words logString of
      ("E":severity:timestamp:rest)
        | elem (read severity :: Int) [1..100] && (>) (read timestamp) 0 ->
          LogMessage (Error (read severity :: Int)) (read timestamp :: Int) (getMessageString rest)
      (msgType:timestamp:rest)
        | elem msgType ["I", "W"] && (>) (read timestamp :: Int) 0 ->
          LogMessage (getMessageType msgType) (read timestamp :: Int) (getMessageString rest)
        | otherwise -> Unknown "some other error"
      errorInput -> Unknown (getMessageString errorInput)


parse :: String -> [LogMessage]
parse logs =
  [parseMessage line | line <- lines logs]

testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file


sortMessages :: [LogMessage] -> MessageTree -> MessageTree
sortMessages [] messageTree = messageTree
sortMessages (x:xs) messageTree =
 sortMessages xs (insert x messageTree)

printTopMessage :: MessageTree -> LogMessage
printTopMessage (Node _ logMessage _) = logMessage
printTopMessage Leaf = Unknown "Hello"


build :: [LogMessage] -> MessageTree
build logMessages =
  sortMessages logMessages Leaf


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert incomingLogMessage Leaf = Node Leaf incomingLogMessage Leaf
insert incoming@(LogMessage messageType incomingTimestamp message) currentMessageTree@(Node left currentMessage@(LogMessage _ timestampCurrentMessage _) right)
  | incomingTimestamp == timestampCurrentMessage = Node left (LogMessage messageType incomingTimestamp message) right
  | incomingTimestamp < timestampCurrentMessage = Node (insert incoming currentMessageTree) currentMessage right
  | incomingTimestamp > timestampCurrentMessage = Node left currentMessage (insert incoming currentMessageTree)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) =
  (inOrder left) ++ [message] ++ (inOrder right)
