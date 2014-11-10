#!/bin/bash


mypath="$(locate -n 1 /coolscripts)"
mypathbuild="$mypath""/../tpm/mainline/dist/build/"
counter=$((0))
for line in $(cat $mypath"/"ips.txt) 
do  
 counter=$((counter+1))
  done
counter=$((counter / 3))
echo "Number is:$counter" 


var=$((1))
for line in $(cat $mypath"/"ips.txt) 
do 
  if [ $((var % 3)) -eq 0 ]
    then
      scp "$mypathbuild""Appraiser/""Appraiser" "$mypathbuild""Attestation/""Attestation" "$mypathbuild""Measurer/""Measurer" "$mypathbuild""PrivacyCA/""PrivacyCA" "$mypathbuild""ScottyCA/""ScottyCA" "root@$line"":"
  fi 
  var=$((var+1))
  done

#hi change
#scp Attestation Appraiser keys.txt root@10.100.0.208: ;
#scp Attestation Appraiser keys.txt root@10.100.0.212: ;

#scp Appraiser apprKeys.txt root@10.100.0.229: ;  #Appraiser
#scp Attestation root@10.100.0.213: ; #Attester
#scp Measurer root@10.100.0.227: ; #Measurer
