{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- | Parse a String into a LogMessage
parseMessage :: String -> LogMessage
parseMessage x =
  case words x of
   "I":time:msg     -> logMsg Info    time msg
   "W":time:msg     -> logMsg Warning time msg
   "E":err:time:msg -> logMsg (Error (read err :: Int)) time msg
   msg              -> Unknown (unwords msg)
  where logMsg typ time msg = LogMessage typ (read time :: Int) (unwords msg)

-- | Parse many LogMessages
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- | Insert a LogMessage into a MessageTree
insert :: LogMessage -> MessageTree -> MessageTree

insert logmsg@(LogMessage _ _ _) Leaf
  = Node Leaf logmsg Leaf
    
insert logmsg@(LogMessage _ t _) (Node l a@(LogMessage _ t0 _) r)
  | t >= t0   = Node l a (insert logmsg r)
  | otherwise = Node (insert logmsg l) a r
                
insert (Unknown _) tree = tree

-- | Build a MessageTree
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- | Convert a sorted MessageTree into [LogMessage]
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l a r) = (inOrder l) ++ [a] ++ (inOrder r)

-- | Find errors with severity of 50 or greater and list them in order
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = foldr extract [] . inOrder . build
  where extract (LogMessage (Error err) _ msg) acc
          | err >= 50 = msg:acc
          | otherwise = acc
        extract _ acc = acc
