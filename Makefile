-include tests/testconfig.mk

.PHONY: test unittests buildtests
test: unittests buildtests

unittests:
	$(RUNHS) -i./src:./unittests unittests/Tests/Development/Cake.hs

buildtests:
	make -C tests