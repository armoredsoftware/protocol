README!!

setup:
1. make sure you are NOT superUser. (if you are, type "su armored" to change back)
2. cd into the coolscripts/ directory
3. execute ./getVMIp.exp to make sure it works
	IF FAILURE:
		are you superUser? don't be. If that doesn't fix it, I don't know, I didn't write this one.
4. Assuming you make it this far, run "./setupaliases.exp"
	IF FAILURE:
		umm.. IDK, make sure you aren't super user..
5. now enter "storeips" (it's a command you just made by running step 4)
	IF FAILURE:
		if you get a "permission denied" on the ips.txt file, cd into coolscripts/ (if you're not there already), and type 
	"sudo chmod 666 ips.txt". This gives everyone read/write permissions. (Alternatively, you can just delete ips.txt file so it becomes created
	/owned by you when you run the script.)
	
	
	
IMPORTANT:
	Be sure to call "storeips" whenever you create or destroy a virtual machine.
	(Otherwise terrible things happen).
	
NOTE: If a script hangs, ctrl-C will get you out
NOTE: If "scpauto" hangs and you have output warning about a keychange for an IP, navigate to your known-keys folder (it tells you where) and delete the entry starting with the bad IP, and ending with '=='.

