module Main where

import qualified AttesterCAComm as A

converseWithScottyCA = A.converseWithScottyCA


main = do
	x <- A.readComp'
	y <- converseWithScottyCA x
	print y


