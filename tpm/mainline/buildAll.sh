#!/bin/bash

cabal build AppraiserProtocol
cabal build AttesterProtocol
cabal build CAMain
#cabal build Measurer


#changed this in protoMonad branch
scp dist/build/AppraiserProtocol/AppraiserProtocol root@10.100.0.234: ;  #Appraiser
scp dist/build/AttesterProtocol/AttesterProtocol root@10.100.0.222: ; #Attester
scp dist/build/AttesterProtocol/AttesterProtocol root@10.100.0.203: ; #Attester
#scp Measurer/Measurer root@10.100.0.227: ; #Measurer
scp dist/build/CAMain/CAMain root@10.100.0.221: ; #PrivacyCA

#scp tpmi/tpmi root@10.100.0.220: ; #Attester(tpmi)
#scp Provisioning/Provisioning root@10.100.0.220: ; #Attester(provision)

