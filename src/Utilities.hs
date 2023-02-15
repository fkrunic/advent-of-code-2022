module Utilities where

note :: e -> Maybe a -> Either e a
note err Nothing = Left err
note _ (Just x) = Right x