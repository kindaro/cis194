module CreditCardValidator where

import Helpers

main = do
    cardNumber <- getLine
    print $ luhnDigit $ read (filter (\x -> elem x ['0'..'9']) cardNumber)

luhnDigit :: Integer -> Integer
luhnDigit n = (\x -> mod x 10) . sum $ map (either snd ((*2) . snd)) $ eiFilter fst (zip (map even [1..]) $ reverse $ numberToDigits n)
    where

luhnCheck = (0 ==) . luhnDigit
