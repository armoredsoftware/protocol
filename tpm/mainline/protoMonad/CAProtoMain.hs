{-# LANGUAGE ScopedTypeVariables #-}

module CAProtoMain where

import ProtoTypes
import ProtoMonad
import ProtoActions


import TPM
import TPMUtil

import Control.Monad.IO.Class
--import qualified Control.Monad.Trans.Reader as T
import Data.Binary
import Data.Digest.Pure.SHA (bytestringDigest, sha1)


import Data.ByteString.Lazy

iPass = tpm_digest_pass aikPass
oPass = tpm_digest_pass ownerPass

caAtt_Mea :: EvidenceDescriptor -> Proto Evidence
caAtt_Mea eds = undefined

caEntity_App :: EvidenceDescriptor -> TPM_PCR_SELECTION -> 
                Proto (Evidence, Nonce, TPM_PCR_COMPOSITE, 
                       (SignedData TPM_PUBKEY), Signature)
caEntity_App d pcrSelect = do
  let nonceA = 34
  send 1 [AEvidenceDescriptor d, ANonce nonceA, ATPM_PCR_SELECTION pcrSelect]
  [AEvidence e, ANonce nA, ATPM_PCR_COMPOSITE pComp, ASignedData (SignedData (ATPM_PUBKEY aikPub) aikSig), ASignature sig] <- receive 1
  
  --do checks here...
  return undefined

caEntity_Att :: Proto ()
caEntity_Att = do
  req@
    [AEvidenceDescriptor dList, 
     reqNonce@(ANonce nApp), 
     ATPM_PCR_SELECTION pcrSelect] <- receive 1
  (iKeyHandle, aikContents) <- caMk_Id
  
  
  
  (ekEncBlob, kEncBlob) <- runWithLinks 
                           [(1, 2)] 
                           (caAtt_CA aikContents)
                           
  sessKey <- caAct_Id iKeyHandle ekEncBlob
  let caCert :: (SignedData TPM_PUBKEY) 
      caCert = decrypt' sessKey kEncBlob 
      
  evidence <- caAtt_Mea dList
  
  let quoteExData = 
        [AEvidence evidence, 
         ANonce nApp, 
         ASignedData $ SignedData (ATPM_PUBKEY (dat caCert)) (sig caCert)]
  (pcrComp, qSig) <- caQuote iKeyHandle pcrSelect quoteExData
  
  let response = 
        [(quoteExData !! 0), 
         (quoteExData !! 1{-(req !! 1)-}), 
         ATPM_PCR_COMPOSITE pcrComp, 
         (quoteExData !! 2), 
         ASignature qSig]
  send 1 response
  
  
  return ()

caAtt_CA :: AikContents -> Proto (CipherText, CipherText)
caAtt_CA signedContents = do
  myInfo <- getEntityInfo 0
  let val = SignedData 
            (ATPM_IDENTITY_CONTENTS  (dat signedContents)) 
            (sig signedContents)
  send 1 [AEntityInfo myInfo, ASignedData val]
  [ACipherText ekEncBlob, ACipherText kEncBlob] <- receive 1
  return (ekEncBlob, kEncBlob)
    
caMk_Id :: Proto (TPM_KEY_HANDLE, AikContents)
caMk_Id = liftIO $ do
  
  (aikHandle, iSig) <- makeAndLoadAIK
  aikPub <- attGetPubKey aikHandle iPass
  let aikContents = TPM_IDENTITY_CONTENTS iPass aikPub
  
  
  return (aikHandle, SignedData aikContents iSig)
  
caAct_Id :: TPM_KEY_HANDLE -> CipherText -> Proto SymmKey
caAct_Id iKeyHandle actInput = liftIO $ do
  iShn <- tpm_session_oiap tpm
  oShn <- tpm_session_oiap tpm
  sessionKey <- tpm_activateidentity tpm iShn oShn iKeyHandle iPass oPass actInput
  return sessionKey
  
caQuote :: TPM_KEY_HANDLE -> TPM_PCR_SELECTION -> [ArmoredData] -> Proto (TPM_PCR_COMPOSITE, Signature)
caQuote qKeyHandle pcrSelect exDataList = liftIO $ do
  let evBlob = packImpl exDataList
      evBlobSha1 = bytestringDigest $ sha1 evBlob
      
  (comp, sig) <- mkQuote qKeyHandle iPass pcrSelect evBlobSha1  
  return (comp, sig)
  
  

  
  