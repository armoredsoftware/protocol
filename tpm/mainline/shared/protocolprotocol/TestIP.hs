module TestIP where

import Network.Info


main = do
    ns <- getNetworkInterfaces
    mapM_ (putStr . showInterface) ns

showInterface n = name n ++ "\n"
               ++ "  IPv4: " ++ show (ipv4 n) ++ "\n"
               ++ "  IPv6: " ++ show (ipv6 n) ++ "\n"
               ++ "  MAC:  " ++ show (mac n) ++ "\n"