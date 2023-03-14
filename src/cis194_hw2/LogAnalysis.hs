{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log


-- Exercise 1
parseMessage :: String -> LogMessage 
parseMessage str =
  case words str of 
    ("E":sev:ts:msg) -> LogMessage (Error (read sev)) (read ts) (unwords msg)
    ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
    ("W":ts:msg) -> LogMessage Info (read ts) (unwords msg)
    unk -> Unknown (unwords unk)

parse :: String -> [LogMessage]
parse str = map parseMessage (filter (not . null) (lines str))


-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert insert_log_msg Leaf = Node Leaf insert_log_msg Leaf
insert insert_log_msg@(LogMessage _ insert_ts _) (Node left node_msg@(LogMessage _ node_ts _) right)
  | insert_ts > node_ts = Node left node_msg (insert insert_log_msg right)
  | insert_ts < node_ts = Node (insert insert_log_msg left) node_msg right
insert _ msg_tree = msg_tree


-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


-- Exercise 4
inOrder ::  MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left log_msg right) = inOrder left ++ [log_msg] ++ inOrder right


-- Exercise 5
filterHelper :: LogMessage -> Bool
filterHelper (LogMessage (Error sev) _ _) = sev >= 50
filterHelper _ = False

messageExtractor :: LogMessage -> Maybe String
messageExtractor (LogMessage _ _ msg) = Just msg
messageExtractor _ = Nothing

whatWentWrong :: [LogMessage] -> [Maybe String]
whatWentWrong = map messageExtractor . filter filterHelper . inOrder . build
