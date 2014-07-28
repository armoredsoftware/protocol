#!/bin/bash
#hi change
#scp Attestation Appraiser keys.txt root@10.100.0.208: ;
#scp Attestation Appraiser keys.txt root@10.100.0.212: ;

scp Appraiser Attestation attKeys.txt Measurer apprKeys.txt root@10.100.234: ; 
scp Appraiser Attestation attKeys.txt Measurer apprKeys.txt root@10.100.208: ; 
scp Appraiser Attestation attKeys.txt Measurer apprKeys.txt root@10.100.205: ;  #Appraiser

