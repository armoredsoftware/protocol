{-# LANGUAGE OverloadedStrings #-}
module Main where



import Web.Scotty
import ScottyCA
main = do scottyCAMain
--main = scotty 3000 $ do
  --  get "/" $ text "foobar"
