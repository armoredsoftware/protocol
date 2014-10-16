#!/bin/bash

cabal build Appraiser
cabal build Attestation
cabal build PrivacyCA
cabal build Measurer

#scp Appraiser/Appraiser root@10.100.0.234: ;  #Appraiser
#scp Attestation/Attestation root@10.100.0.220: ; #Attester
#scp Measurer/Measurer root@10.100.0.227: ; #Measurer
#scp PrivacyCA/PrivacyCA root@10.100.0.221: ; #PrivacyCA

#scp tpmi/tpmi root@10.100.0.220: ; #Attester(tpmi)
#scp Provisioning/Provisioning root@10.100.0.220: ; #Attester(provision)

