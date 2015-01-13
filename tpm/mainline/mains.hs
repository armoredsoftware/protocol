--Appraiser Main:
main = do 
  putStrLn "START main of Appraiser"
  (pcrSelect, nonce) <- mkTPMRequest ([0..23]::[Word8])
  let mReq = mkMeasureReq [0..2]
      req = (Request mReq pcrSelect nonce)
  chan <- sendRequest req
  response <- receiveResponse chan
  result <- evaluate req response
  showEvalResult result
  
  
  --Attester Main:
main = do 
  putStrLn "START main of Attestation"
  ...
  tpm_takeownership tpm tkShn pubkey oPass sPass
  req <- liftIO $ receiveRequest apprChan
  iKeyHandle <- liftIO $ createAndLoadIdentKey
  ...
  caCert <- getCACert iKeyHandle caChan
  resp <- mkResponse req caCert iKeyHandle iPass
          eList <- getEvidence desiredE
          let evBlob = ePack eList nonce caCert
          quote <- mkQuote qKeyHandle qKeyPass pcrSelect evBlobSha1
  sendResponse apprChan resp