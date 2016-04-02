module Helpers (eiFilter, eiMap, numberToDigits) where

eiFilter :: (a -> Bool) -> [a] -> [Either a a]
eiFilter f xs = map (conditional f) xs
    where
    conditional f x
        | f x = Right x
        | otherwise = Left x

eiMap :: (a -> c) -> (b -> d) -> [Either a b] -> [Either c d]
eiMap _ _ [] = []
eiMap fL fR (x:xs) = f x : eiMap fL fR xs
    where
    f (Left x) = Left $ fL x
    f (Right y) = Right $ fR y

numberToDigits :: Integer -> [Integer]
numberToDigits = (map (read . return)) . show
