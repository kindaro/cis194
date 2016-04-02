module CreditCardValidator where

import Control.Monad
import Control.Applicative

import Helpers

main :: IO ()
main = do
    liftM (luhnDigit . read . filterDigits) getLine >>= print
    where
        filterDigits = filter  (`elem` ['0'..'9'])

luhnDigit :: Integer -> Integer
luhnDigit n = mod10 . sum . concat . (map numberToDigits) . (map doubleRights) $ eiEven $ reverse $ numberToDigits n
    where
        mod10 = flip mod 10
        doubleRights = either id (*2)
        eiEven = (map (either (Left . snd) (Right . snd))) . (eiFilter fst) . zip (even <$> [1..])

luhnCheck = (0 ==) . luhnDigit
