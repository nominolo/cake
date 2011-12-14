include ../testconfig.mk

ROOT=../..

.PHONY: test
test:
	echo "foo" > A.txt
	echo "bar" > B.txt
	echo "CcC" > C.txt
	echo "dDd" > D.txt
	@echo "=== First Build ==="
	$(RUNHS) -i$(ROOT)/src cakefile.hs
	echo "foo\nbar" > AB.txt.expected
	echo "CcC\ndDd" > CD.txt.expected
	echo "foo\nbar\nCcC\ndDd" > ABCD.txt.expected
	diff -u AB.txt AB.txt.expected
	diff -u CD.txt CD.txt.expected
	diff -u ABCD.txt ABCD.txt.expected
	@echo "=== Incremental Rebuild ==="
	$(RUNHS) -i$(ROOT)/src cakefile.hs
	diff -u AB.txt AB.txt.expected
	diff -u CD.txt CD.txt.expected
	diff -u ABCD.txt ABCD.txt.expected

	@echo "=== Modify Leaf ==="
	echo "FOO" >> A.txt
	$(RUNHS) -i$(ROOT)/src cakefile.hs
	echo "foo\nFOO\nbar" > AB.txt.expected
	echo "CcC\ndDd" > CD.txt.expected
	echo "foo\nFOO\nbar\nCcC\ndDd" > ABCD.txt.expected
	diff -u AB.txt AB.txt.expected
	diff -u CD.txt CD.txt.expected
	diff -u ABCD.txt ABCD.txt.expected

	@echo "=== Modify Two Leaves ==="
	echo "BAR" > B.txt
	echo "EEE" >> D.txt
	$(RUNHS) -i$(ROOT)/src cakefile.hs
	echo "foo\nFOO\nBAR" > AB.txt.expected
	echo "CcC\ndDd\nEEE" > CD.txt.expected
	echo "foo\nFOO\nBAR\nCcC\ndDd\nEEE" > ABCD.txt.expected
	diff -u AB.txt AB.txt.expected
	diff -u CD.txt CD.txt.expected
	diff -u ABCD.txt ABCD.txt.expected

	@echo "=== Cleaning Up ==="
	rm A.txt B.txt AB.txt AB.txt.expected C.txt D.txt CD.txt \
		CD.txt.expected ABCD.txt ABCD.txt.expected .cake-db
