include ../testconfig.mk

ROOT=../..

.PHONY: test
test:
	echo "foo" > A.txt
	echo "bar" > B.txt
	$(RUNHS) -i$(ROOT)/src cakefile.hs
	echo "foo\nbar" > AB.txt.expected
	diff -u AB.txt AB.txt.expected
	$(RUNHS) -i$(ROOT)/src cakefile.hs
	diff -u AB.txt AB.txt.expected
	echo "FOO" >> A.txt
	$(RUNHS) -i$(ROOT)/src cakefile.hs
	echo "foo\nFOO\nbar" > AB.txt.expected
	diff -u AB.txt AB.txt.expected
	rm A.txt B.txt AB.txt AB.txt.expected .cake-db
