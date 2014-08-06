JSONCaster.hs:1:1: Splicing declarations
    deriveJSON defaultOptions ''EvidenceDescriptor
  ======>
    JSONCaster.hs:19:3-48
    instance ToJSON EvidenceDescriptor where
      toJSON
        = \ value_a7k9
            -> case value_a7k9 of {
                 D0 -> String (pack "D0")
                 D1 -> String (pack "D1")
                 D2 -> String (pack "D2") }
    instance FromJSON EvidenceDescriptor where
      parseJSON
        = \ value_a7ka
            -> case value_a7ka of {
                 String txt_a7kb
                   | (txt_a7kb == (pack "D0")) -> Control.Applicative.pure D0
                   | (txt_a7kb == (pack "D1")) -> Control.Applicative.pure D1
                   | (txt_a7kb == (pack "D2")) -> Control.Applicative.pure D2
                   | otherwise
                   -> Data.Aeson.TH.noMatchFail
                        "Demo2Shared.EvidenceDescriptor" (unpack txt_a7kb)
                 other_a7kc
                   -> Data.Aeson.TH.noStringFail
                        "Demo2Shared.EvidenceDescriptor"
                        (Data.Aeson.TH.valueConName other_a7kc) }
JSONCaster.hs:1:1: Splicing declarations
    deriveJSON defaultOptions ''EvidenceDescriptorW
  ======>
    JSONCaster.hs:20:3-49
    instance ToJSON EvidenceDescriptorW where
      toJSON
        = \ value_a7rd
            -> case value_a7rd of {
                 EDW arg1_a7re
                   -> object [((pack "evidenceDescriptor") .= arg1_a7re)] }
    instance FromJSON EvidenceDescriptorW where
      parseJSON
        = \ value_a7rf
            -> case value_a7rf of {
                 Object recObj_a7rg
                   -> (EDW
                       Data.Functor.<$>
                         (Data.Aeson.TH.lookupField
                            "JSONCaster.EvidenceDescriptorW"
                            "EDW"
                            recObj_a7rg
                            (pack "evidenceDescriptor")))
                 other_a7rh
                   -> Data.Aeson.TH.parseTypeMismatch'
                        "EDW"
                        "JSONCaster.EvidenceDescriptorW"
                        "Object"
                        (Data.Aeson.TH.valueConName other_a7rh) }
JSONCaster.hs:1:1: Splicing declarations
    deriveJSON defaultOptions ''DesiredEvidence
  ======>
    JSONCaster.hs:21:3-45
    instance ToJSON DesiredEvidence where
      toJSON
        = \ value_a7sE
            -> case value_a7sE of {
                 DesiredEvidence arg1_a7sF
                   -> object [((pack "evidenceDescriptorList") .= arg1_a7sF)] }
    instance FromJSON DesiredEvidence where
      parseJSON
        = \ value_a7sG
            -> case value_a7sG of {
                 Object recObj_a7sH
                   -> (DesiredEvidence
                       Data.Functor.<$>
                         (Data.Aeson.TH.lookupField
                            "Demo2Shared.DesiredEvidence"
                            "DesiredEvidence"
                            recObj_a7sH
                            (pack "evidenceDescriptorList")))
                 other_a7sI
                   -> Data.Aeson.TH.parseTypeMismatch'
                        "DesiredEvidence"
                        "Demo2Shared.DesiredEvidence"
                        "Object"
                        (Data.Aeson.TH.valueConName other_a7sI) }
JSONCaster.hs:1:1: Splicing declarations
    deriveJSON defaultOptions ''EvidencePiece
  ======>
    JSONCaster.hs:22:3-43
    instance ToJSON EvidencePiece where
      toJSON
        = \ value_a7t8
            -> case value_a7t8 of {
                 M0 arg1_a7t9
                   -> object
                        (((pack "tag") .= (String (pack "M0")))
                         GHC.Types.: [((pack "m0Rep") .= arg1_a7t9)])
                 M1 arg1_a7ta
                   -> object
                        (((pack "tag") .= (String (pack "M1")))
                         GHC.Types.: [((pack "m1Rep") .= arg1_a7ta)])
                 M2 arg1_a7tb
                   -> object
                        (((pack "tag") .= (String (pack "M2")))
                         GHC.Types.: [((pack "m2Rep") .= arg1_a7tb)]) }
    instance FromJSON EvidencePiece where
      parseJSON
        = \ value_a7tc
            -> case value_a7tc of {
                 Object obj_a7td
                   -> do { conKey_a7te <- (obj_a7td .: (pack "tag"));
                           case conKey_a7te of {
                             _ | (conKey_a7te == (pack "M0"))
                               -> (M0
                                   Data.Functor.<$>
                                     (Data.Aeson.TH.lookupField
                                        "Demo2Shared.EvidencePiece" "M0" obj_a7td (pack "m0Rep")))
                               | (conKey_a7te == (pack "M1"))
                               -> (M1
                                   Data.Functor.<$>
                                     (Data.Aeson.TH.lookupField
                                        "Demo2Shared.EvidencePiece" "M1" obj_a7td (pack "m1Rep")))
                               | (conKey_a7te == (pack "M2"))
                               -> (M2
                                   Data.Functor.<$>
                                     (Data.Aeson.TH.lookupField
                                        "Demo2Shared.EvidencePiece" "M2" obj_a7td (pack "m2Rep")))
                               | otherwise
                               -> Data.Aeson.TH.conNotFoundFailTaggedObject
                                    "Demo2Shared.EvidencePiece"
                                    ["M0", "M1", "M2"]
                                    (unpack conKey_a7te) } }
                 other_a7tf
                   -> Data.Aeson.TH.noObjectFail
                        "Demo2Shared.EvidencePiece"
                        (Data.Aeson.TH.valueConName other_a7tf) }
JSONCaster.hs:1:1: Splicing declarations
    deriveJSON defaultOptions ''Quote
  ======>
    JSONCaster.hs:23:3-35
    instance ToJSON Quote where
      toJSON
        = \ value_a7uD
            -> case value_a7uD of {
                 Quote arg1_a7uE arg2_a7uF arg3_a7uG
                   -> object
                        [((pack "pcrList") .= arg1_a7uE),
                         ((pack "nonceQuote") .= arg2_a7uF),
                         ((pack "signatureQuote") .= arg3_a7uG)] }
    instance FromJSON Quote where
      parseJSON
        = \ value_a7uH
            -> case value_a7uH of {
                 Object recObj_a7uI
                   -> (((Quote
                         Data.Functor.<$>
                           (Data.Aeson.TH.lookupField
                              "Demo2Shared.Quote" "Quote" recObj_a7uI (pack "pcrList")))
                        Control.Applicative.<*>
                          (Data.Aeson.TH.lookupField
                             "Demo2Shared.Quote" "Quote" recObj_a7uI (pack "nonceQuote")))
                       Control.Applicative.<*>
                         (Data.Aeson.TH.lookupField
                            "Demo2Shared.Quote" "Quote" recObj_a7uI (pack "signatureQuote")))
                 other_a7uJ
                   -> Data.Aeson.TH.parseTypeMismatch'
                        "Quote"
                        "Demo2Shared.Quote"
                        "Object"
                        (Data.Aeson.TH.valueConName other_a7uJ) }
JSONCaster.hs:1:1: Splicing declarations
    deriveJSON defaultOptions ''Evidence
  ======>
    JSONCaster.hs:24:3-38
    instance ToJSON Evidence where
      toJSON
        = \ value_a7vy
            -> case value_a7vy of {
                 Evidence arg1_a7vz
                   -> object [((pack "evidencePieceList") .= arg1_a7vz)] }
    instance FromJSON Evidence where
      parseJSON
        = \ value_a7vA
            -> case value_a7vA of {
                 Object recObj_a7vB
                   -> (Evidence
                       Data.Functor.<$>
                         (Data.Aeson.TH.lookupField
                            "Demo2Shared.Evidence"
                            "Evidence"
                            recObj_a7vB
                            (pack "evidencePieceList")))
                 other_a7vC
                   -> Data.Aeson.TH.parseTypeMismatch'
                        "Evidence"
                        "Demo2Shared.Evidence"
                        "Object"
                        (Data.Aeson.TH.valueConName other_a7vC) }
JSONCaster.hs:1:1: Splicing declarations
    deriveJSON defaultOptions ''Request
  ======>
    JSONCaster.hs:25:3-37
    instance ToJSON Request where
      toJSON
        = \ value_a7w8
            -> case value_a7w8 of {
                 Request arg1_a7w9 arg2_a7wa arg3_a7wb
                   -> object
                        [((pack "desiredEvidence") .= arg1_a7w9),
                         ((pack "tpmRequest") .= arg2_a7wa),
                         ((pack "nonceRequest") .= arg3_a7wb)] }
    instance FromJSON Request where
      parseJSON
        = \ value_a7wc
            -> case value_a7wc of {
                 Object recObj_a7wd
                   -> (((Request
                         Data.Functor.<$>
                           (Data.Aeson.TH.lookupField
                              "Demo2Shared.Request"
                              "Request"
                              recObj_a7wd
                              (pack "desiredEvidence")))
                        Control.Applicative.<*>
                          (Data.Aeson.TH.lookupField
                             "Demo2Shared.Request" "Request" recObj_a7wd (pack "tpmRequest")))
                       Control.Applicative.<*>
                         (Data.Aeson.TH.lookupField
                            "Demo2Shared.Request" "Request" recObj_a7wd (pack "nonceRequest")))
                 other_a7we
                   -> Data.Aeson.TH.parseTypeMismatch'
                        "Request"
                        "Demo2Shared.Request"
                        "Object"
                        (Data.Aeson.TH.valueConName other_a7we) }
JSONCaster.hs:1:1: Splicing declarations
    deriveJSON defaultOptions ''Response
  ======>
    JSONCaster.hs:26:3-38
    instance ToJSON Response where
      toJSON
        = \ value_a7xb
            -> case value_a7xb of {
                 Response arg1_a7xc arg2_a7xd
                   -> object
                        [((pack "evidencePackage") .= arg1_a7xc),
                         ((pack "quotePackage") .= arg2_a7xd)] }
    instance FromJSON Response where
      parseJSON
        = \ value_a7xe
            -> case value_a7xe of {
                 Object recObj_a7xf
                   -> ((Response
                        Data.Functor.<$>
                          (Data.Aeson.TH.lookupField
                             "Demo2Shared.Response"
                             "Response"
                             recObj_a7xf
                             (pack "evidencePackage")))
                       Control.Applicative.<*>
                         (Data.Aeson.TH.lookupField
                            "Demo2Shared.Response"
                            "Response"
                            recObj_a7xf
                            (pack "quotePackage")))
                 other_a7xg
                   -> Data.Aeson.TH.parseTypeMismatch'
                        "Response"
                        "Demo2Shared.Response"
                        "Object"
                        (Data.Aeson.TH.valueConName other_a7xg) }
JSONCaster.hs:1:1: Splicing declarations
    deriveJSON defaultOptions ''EvidencePackage
  ======>
    JSONCaster.hs:27:3-45
    instance ToJSON EvidencePackage where
      toJSON
        = \ value_a7xP
            -> case value_a7xP of {
                 EvidencePackage arg1_a7xQ arg2_a7xR arg3_a7xS
                   -> object
                        [((pack "evidence") .= arg1_a7xQ),
                         ((pack "nonceEvidencePackage") .= arg2_a7xR),
                         ((pack "signatureEvidencePackage") .= arg3_a7xS)] }
    instance FromJSON EvidencePackage where
      parseJSON
        = \ value_a7xT
            -> case value_a7xT of {
                 Object recObj_a7xU
                   -> (((EvidencePackage
                         Data.Functor.<$>
                           (Data.Aeson.TH.lookupField
                              "Demo2Shared.EvidencePackage"
                              "EvidencePackage"
                              recObj_a7xU
                              (pack "evidence")))
                        Control.Applicative.<*>
                          (Data.Aeson.TH.lookupField
                             "Demo2Shared.EvidencePackage"
                             "EvidencePackage"
                             recObj_a7xU
                             (pack "nonceEvidencePackage")))
                       Control.Applicative.<*>
                         (Data.Aeson.TH.lookupField
                            "Demo2Shared.EvidencePackage"
                            "EvidencePackage"
                            recObj_a7xU
                            (pack "signatureEvidencePackage")))
                 other_a7xV
                   -> Data.Aeson.TH.parseTypeMismatch'
                        "EvidencePackage"
                        "Demo2Shared.EvidencePackage"
                        "Object"
                        (Data.Aeson.TH.valueConName other_a7xV) }
JSONCaster.hs:1:1: Splicing declarations
    deriveJSON defaultOptions ''QuotePackage
  ======>
    JSONCaster.hs:28:3-42
    instance ToJSON QuotePackage where
      toJSON
        = \ value_a7yE
            -> case value_a7yE of {
                 QuotePackage arg1_a7yF arg2_a7yG arg3_a7yH
                   -> object
                        [((pack "quoteQuotePackage") .= arg1_a7yF),
                         ((pack "hashQuotePackage") .= arg2_a7yG),
                         ((pack "signatureQuotePackage") .= arg3_a7yH)] }
    instance FromJSON QuotePackage where
      parseJSON
        = \ value_a7yI
            -> case value_a7yI of {
                 Object recObj_a7yJ
                   -> (((QuotePackage
                         Data.Functor.<$>
                           (Data.Aeson.TH.lookupField
                              "Demo2Shared.QuotePackage"
                              "QuotePackage"
                              recObj_a7yJ
                              (pack "quoteQuotePackage")))
                        Control.Applicative.<*>
                          (Data.Aeson.TH.lookupField
                             "Demo2Shared.QuotePackage"
                             "QuotePackage"
                             recObj_a7yJ
                             (pack "hashQuotePackage")))
                       Control.Applicative.<*>
                         (Data.Aeson.TH.lookupField
                            "Demo2Shared.QuotePackage"
                            "QuotePackage"
                            recObj_a7yJ
                            (pack "signatureQuotePackage")))
                 other_a7yK
                   -> Data.Aeson.TH.parseTypeMismatch'
                        "QuotePackage"
                        "Demo2Shared.QuotePackage"
                        "Object"
                        (Data.Aeson.TH.valueConName other_a7yK) }
