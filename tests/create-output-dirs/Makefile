include ../testconfig.mk

ROOT=../..

.PHONY: test
test:
	echo "foo" > A.txt
	$(RUNHS) -i$(ROOT)/src cakefile.hs
	diff -u A.txt build/A.txt
	$(RUNHS) -i$(ROOT)/src cakefile.hs
	diff -u A.txt build/A.txt
	echo "bar" >> A.txt
	$(RUNHS) -i$(ROOT)/src cakefile.hs
	diff -u A.txt build/A.txt
	rm A.txt build/A.txt .cake-db
	rm -r build
