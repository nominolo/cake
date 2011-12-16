include ../testconfig.mk

ROOT=../..

.PHONY: test
test:
	echo "foo" > A.txt
	$(RUNHS) -i$(ROOT)/src cakefile.hs 2> stderr.actual || true
	diff -u stderr.actual stderr.expected
	rm -f A.txt .cake-db stderr.actual