{-# LANGUAGE ScopedTypeVariables #-}

module ProtoMain where

import ProtoTypes
import ProtoMonad
import ProtoActions

import Data.ByteString.Lazy

nsEntityA :: Proto Nonce
nsEntityA = do
  nonceA <- generateNonce
  myInfo <- getEntityInfo 0  --0 is MY id by default
  send 1 [ANonce nonceA, AEntityInfo myInfo]
  [ACipherText cipherText] <- receive 1
  [ANonce nA, ANonce nB, AEntityInfo eInfo] <- decrypt cipherText
  --Check nonceA == nA here?
  cipherOut <- encrypt 1 [ANonce nB]
  send 1 [ACipherText cipherOut]
  return nB

nsEntityB :: Proto Nonce
nsEntityB = do
  [ACipherText cipherMessage1] <- receive 1
  [ANonce nA, AEntityInfo eInfo] <- decrypt cipherMessage1
  nonceB <- generateNonce
  myInfo <- getEntityInfo 0  --0 is MY id by default
  cipherOut <- encrypt 1 [ANonce nA, ANonce nonceB, AEntityInfo myInfo]
  send 1 [ACipherText cipherOut]
  [ACipherText cipherMessage2] <- receive 1
  [ANonce nB] <- decrypt cipherMessage2
  --Check nonceB == nB here?
  return nA

