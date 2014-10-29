#!/bin/bash
#hi change
#scp Attestation Appraiser keys.txt root@10.100.0.208: ;
#scp Attestation Appraiser keys.txt root@10.100.0.212: ;

scp dist/build/Appraiser/Appraiser root@10.100.0.234: ;  #Appraiser
scp dist/build/Attestation/Attestation root@10.100.0.220: ; #Attester
#scp dist/buildMeasurer/Measurer root@10.100.0.227: ; #Measurer
#scp dist/build/PrivacyCA/PrivacyCA root@10.100.0.221: ; #PrivacyCA

#scp tpmi/tpmi root@10.100.0.220: ; #Attester(tpmi)
#scp dist/build/Provisioning/Provisioning root@10.100.0.220: ; #Attester(provision)

