#!/bin/bash
#hi change
#scp Attestation Appraiser keys.txt root@10.100.0.208: ;
#scp Attestation Appraiser keys.txt root@10.100.0.212: ;

#scp dist/build/AMain/AMain root@10.100.0.234: ;  #A
#scp dist/build/BMain/BMain root@10.100.0.222: ; #B

scp dist/build/AttesterMain/AttesterMain root@10.100.0.222: ;
scp dist/build/AppMain/AppMain root@10.100.0.234: ;
scp dist/build/CAMain/CAMain root@10.100.0.221: ;



#scp dist/build/Appraiser/Appraiser root@10.100.0.234: ;  #Appraiser
#scp dist/build/Attestation/Attestation root@10.100.0.222: ; #Attester
#scp dist/build/PrivacyCA/PrivacyCA root@10.100.0.221: ; #PrivCA
#scp dist/build/Measurer/Measurer root@10.100.0.240: ; #Measurer

