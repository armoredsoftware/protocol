#!/bin/bash

cabal build AppMain
cabal build AttesterMain
cabal build CAMain
#cabal build Measurer



scp dist/build/AppMain/AppMain root@10.100.0.234: ;  #Appraiser
scp dist/build/AttesterMain/AttesterMain root@10.100.0.222: ; #Attester
scp dist/build/CAMain/CAMain root@10.100.0.221: ; #PrivacyCA

#scp tpmi/tpmi root@10.100.0.220: ; #Attester(tpmi)
#scp Provisioning/Provisioning root@10.100.0.220: ; #Attester(provision)

