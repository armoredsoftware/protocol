module KeyUtil where

-- utility libraries
import System.IO
import System.IO.Unsafe (unsafePerformIO)

--crypto libraries
import Crypto.Random
import Crypto.PubKey.RSA

getKeys :: (PrivateKey, PublicKey)
getKeys = unsafePerformIO $ readKeys

getPriKey :: PrivateKey
getPriKey = fst getKeys

getPubKey :: PublicKey
getPubKey = snd getKeys

readKeys :: IO (PrivateKey, PublicKey)
readKeys =
     do handle <- openFile "keys.txt" ReadMode
        priString <- hGetLine handle
        pubString <- hGetLine handle
        let pri :: PrivateKey
            pri = read priString
            pub :: PublicKey
            pub = read pubString
        hClose handle
        return (pri, pub)



--Utility function to be used ONCE to generate keys and ouput them to keys.txt
exportKeys :: IO ()
exportKeys  =
     do e <- createEntropyPool
        let gen :: SystemRNG
            gen = cprgCreate e
            ((pub, pri), _) = generate gen 255 3
        doExport pri pub
        putStrLn "Created file keys.txt"

--Helper for exportKeys
doExport :: PrivateKey -> PublicKey ->  IO ()
doExport pri pub =
                   do handle <- openFile "keys.txt" WriteMode
                      hPutStrLn handle $ show pri
                      hPutStrLn handle $ show pub
                      hClose handle
           

