{-# LANGUAGE RecursiveDo #-}

module Exploration.ValueRecursion where

import Data.IORef

data Node = Node Int (IORef Node)

-- Node that points back to itself
mkNode :: IO (IORef Node)
mkNode = do 
  rec p <- newIORef (Node 0 p)
  putStrLn "Node created"
  return p

-- Two nodes pointing to each-other
mk2Nodes :: IO (IORef Node)
mk2Nodes = do 
  rec p <- newIORef (Node 0 r)
      r <- newIORef (Node 1 p)
  putStrLn "Nodes created"
  return p

node_repsum :: Node -> IO Node
node_repsum n = do 
  rec (m, s) <- node_rep_x_sum n s
  putStrLn ""
  return m

node_rep_x_sum :: Node -> Int -> IO (Node, Int)
node_rep_x_sum (Node i ref) s = do 
  m <- readIORef ref
  (before, sumBefore) <- node_rep_x_sum m s
  putStr (show i)
  b <- readIORef before
  return (Node s b, i + sumBefore)

{-
main :: IO ()
main = do 
  p <- mk2Nodes

  Node x q <- readIORef p
  print x

  Node y _ <- readIORef q
  print y
-}

data BTree 
  = Z 
  | B Int BTree BTree 
  deriving Show

repsum :: BTree -> IO BTree
repsum t = do 
  rec (u, s) <- rep_x_sum t s
  putStrLn ""
  return u

rep_x_sum :: BTree -> Int -> IO (BTree, Int)
rep_x_sum Z _ = return (Z, 0)
rep_x_sum (B i left right) s = do
  putStr "("
  (_, sumLeft) <- rep_x_sum left s
  putStr (show i)
  (_, sumRight) <- rep_x_sum right s
  putStr ")"
  return (B s left right, i + sumLeft + sumRight)

main :: IO ()
main = 
  repsum (B 4 (B 3 Z Z) (B 5 Z (B 1 Z Z)))
    >>= print
