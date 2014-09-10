module Main where
import Provisioning

main :: IO ()
main = do 
  putStrLn "START of provisioning main"
  compGolden <- getCurrentComp
  doExport goldenFileName compGolden
  --return ()
          
          
