#!/bin/bash
#hi change
#scp Attestation Appraiser keys.txt root@10.100.0.208: ;
#scp Attestation Appraiser keys.txt root@10.100.0.212: ;

scp dist/build/Appraiser/Appraiser root@10.100.0.234: ;  #Appraiser
scp dist/build/Attestation/Attestation root@10.100.0.222: ; #Attester
#scp dist/build/PrivacyCA/PrivacyCA root@10.100.0.221: ; #PrivCA
scp dist/build/Measurer/Measurer root@10.100.0.240: ; #Measurer

