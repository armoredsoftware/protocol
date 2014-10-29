#!/bin/bash
#IMPORTANT execute 'updatedb' in the terminal if this does not locate correctly
OUTPUT="$(locate -n 1 coolscripts/getVMIp.exp)"
$OUTPUT > ips.txt
#./getVMIp.exp > ips.txt
#/home/armored/scp.sh


