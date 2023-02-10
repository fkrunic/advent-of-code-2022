module Day13 where

data Comparison 
  = CInt Int 
  | CList [Comparison]

valid :: Comparison -> Comparison -> Bool
valid (CInt i) (CInt j) = i <= j
valid (CList []) (CList _) = True
valid (CList _) (CList []) = False
valid (CList (x:xs)) (CList (y:ys)) = valid x y && valid (CList xs) (CList ys)
valid lone@(CInt _) xs = valid (CList [lone]) xs
valid xs lone@(CInt _) = valid xs (CList [lone])