PREPARATION

1. Set path to 'common' and 'include' in ./relay/Makefile. (This is not necessary if you 'code' directory is in the same directory as 'protocol')

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
	
