PREPARATION

1. Set path to 'common' and 'include' in ./relay/Makefile. (This is not necessary if you 'code' directory is in the same directory as 'protocol')

THE FAKE ATTESTER

BUILD

	$make build-fakeattester
RUN

	$make run-fakeattester DOMID=<relay domid>

THE RELAY

BUILD

	$make build-relay
RUN

	$make run-relay DOMID=<attester domid>


THE MEASURER (hotspot)

BUILD

	$make build-hotspot
RUN

	$make run-hotspot APP=<path to application jar>
	
	OR
	
	$make run-hotspot-hackableloop
	
