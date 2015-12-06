.PHONY: test test1 test2 test3 test4 all

all: test

test1:
	node task1.js

test2:
	node task2.js

test3:
	python -m unittest task3

test4:
	sbcl --load task4.lisp --eval "(run-tests)" --quit

test: test1 test2 test3 test4
