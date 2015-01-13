{-# LANGUAGE OverloadedStrings #-}
module Main where

import ScottyCA
import Web.Scotty
import System.Environment
import Text.Regex
import Text.Read

defaultPort = 3000
version = "0.0.01"
main = do 
	args <- getArgs
	helpprinted <- handleHelp args
	case helpprinted of
	  False -> do
		port <- handlePort args
	    	verbosity <- handleVerbosity args
	    	--putStrLn ("port: " ++ (show port) ++ " verbosity: " ++ (show verbosity))
	    	putStrLn ("CA version: " ++ version)
	    	scottyCAMain port verbosity	  
	  True  -> do
	    return ()

	
handleHelp :: [String] -> IO Bool
handleHelp args = do
		  let regex = mkRegex "-[Hh][Ee][Ll][Pp]"
		  case (findFirstMatch regex args) of
		    Nothing -> return False
		    Just _ -> do
		    		  putStrLn "\n"
		  		  putStrLn "Can enter the port number and verbosity:"
		  		  putStrLn ""
		  		  putStrLn "-o n -> no output"
		  		  putStrLn "-o b -> basic output"
		  		  putStrLn "-o v -> verbose output"
		  		  putStrLn ""
		  		  putStrLn ("If no port is given (-p <NUM>) defaults to " ++ (show defaultPort))
		  		  putStrLn "If output level not specified, defaults to b (basic)."
		  		  putStrLn "\n\n\n"
		  		  return True
handlePort :: [String] -> IO Int
handlePort args = do
		    let regex = mkRegex "-[Pp]"
		    case (findFirstMatch regex args) of
		      Nothing -> return defaultPort
		      Just ls -> case ls of
		      		  (x1:x2:_) -> case readMaybe x2 :: Maybe Int of
		      		                Just val -> return val
		      		                Nothing -> do
		      		                	    putStrLn ( x2 ++ " is an invalid port. Using default port: " ++ (show defaultPort))
		      		                	    
		      		                	    return defaultPort
		      		  _         -> do
		      		  		putStrLn ("No port given. Using default port: " ++ (show defaultPort))
		      		  		return defaultPort

vToi :: String -> Maybe Int
vToi char | char == "n" || char == "N" = Just 0
	  | char == "b" || char == "B" = Just 1
	  | char == "v" || char == "V" = Just 2
	  | otherwise = Nothing
	  
handleVerbosity :: [String] -> IO Int
handleVerbosity args = do
		    let regex = mkRegex "-[Oo]"
		    case (findFirstMatch regex args) of
		      Nothing -> return 1
		      Just ls -> case ls of
		      		  (x1:x2:_) -> case (vToi x2) of
		      		  		Nothing -> do
		      		  			 putStrLn (x2 ++ " is an invalid verbosity. Using b (basic) instead.")
		      		  			 return 1
		      		  		(Just val)  -> return val
		      		  _         -> do
		      		  		putStrLn ("No verbosity specified. Using 'basic' (b).")
		      		  		return 1	
		    				    		
findFirstMatch :: Regex -> [String] -> Maybe [String]
findFirstMatch regex [] = Nothing
findFirstMatch regex (x:xs) = case (matchRegex regex x) of 		    		
				Nothing -> findFirstMatch regex xs
				Just _  -> Just (x:xs)
				
