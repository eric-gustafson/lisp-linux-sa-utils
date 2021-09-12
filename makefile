export SHELL=/bin/bash

LISP_FILES=$(wildcard *.lisp)

clean:
	- rm ut
	- rm mf/ut/lsa/ut
