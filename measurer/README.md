This directory contains the measurer, the relay (vchan<->sockets relay), test applications for measurement, and a fake attester for testing.

The relay and the measurer should execute on the same VM. The attester should be on a separate VM in the same compute node. The attester will comunicate directly to the relay as if it were the measurer (is not aware of the measurer as a middle layer).


PREPARATION

1. In a VM, launch the fake attester (or another attester).
2. In a separate VM on the same node, launch the relay and then the measurer. (See below for details)
3. Get talking!

*** You may need to set path to 'common' and 'include' in ./relay/Makefile. (This is not necessary if you 'code' directory is in the same directory as 'protocol') ***

THE FAKE ATTESTER

	$make build-fakeattester
	
	$make run-fakeattester DOMID=<relay domid>

THE RELAY

	$make build-relay

	$make run-relay DOMID=<attester domid>


THE MEASURER (hotspot)

	$make build-hotspot

	$make run-hotspot APP=<path to application jar>
	OR
	$make run-hotspot-hackableloop
	
