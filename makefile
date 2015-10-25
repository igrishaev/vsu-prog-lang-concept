.PHONY: test test1 test2 test3

test1:
	node task1.js

test2:
	node task2.js

test3:
	python -m unittest task3

test: test1 test2 test3
