#!/bin/bash
#IMPORTANT execute 'updatedb' in the terminal if this does not locate correctly
#getvmi="$(locate -n 1 coolscripts/getVMIp.exp)"
mypath="$(locate -n 1 /coolscripts)"
$mypath/getVMIp.exp > $mypath/ips.txt
#$getvmi > ips.txt
#./getVMIp.exp > ips.txt
#/home/armored/scp.sh


