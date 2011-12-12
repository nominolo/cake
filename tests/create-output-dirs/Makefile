include ../testconfig.mk

ROOT=../..

.PHONY: test
test:
	@echo "Hello"
	echo "foo" > A.txt
	$(RUNHS) -i$(ROOT)/src cakefile.hs
	diff -u A.txt build/A.txt
	$(RUNHS) -i$(ROOT)/src cakefile.hs
	diff -u A.txt build/A.txt
	echo "bar" >> A.txt
	$(RUNHS) -i$(ROOT)/src cakefile.hs
	diff -u A.txt build/A.txt
	rm A.txt build/A.txt
	rm -r build