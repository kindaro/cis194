module CreditCardValidator where

import Control.Monad

import Helpers

main :: IO ()
main = liftM (luhnDigit . read . filterDigits) getLine >>= print
    where
        filterDigits = filter (`elem` digits)
        digits = ['0'..'9']

luhnDigit :: Integer -> Integer
luhnDigit = mod10
          . sum
          . concat
          . (map numberToDigits)
          . (map doubleRights)
          . eiEven
          . reverse
          . numberToDigits
    where
        mod10 = flip mod 10
        doubleRights = either id (*2)
        eiEven = (eiMap snd snd) . (eiFilter fst) . zip (map even [1..])

luhnCheck :: Integer -> Bool
luhnCheck = (0 ==) . luhnDigit
