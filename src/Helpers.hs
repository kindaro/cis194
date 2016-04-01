module Helpers (eiFilter, numberToDigits) where

eiFilter :: (a -> Bool) -> [a] -> [Either a a]
eiFilter f xs = map (conditional f) xs
    where
    conditional f x
        | f x = Right x
        | otherwise = Left x

numberToDigits :: Integer -> [Integer]
numberToDigits n = map (read . (:[])) (show n)
