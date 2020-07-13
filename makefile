export SHELL=/bin/bash

LISP_FILES=$(wildcard *.lisp)

ut: ut.ros $(LISP_FILES)
	ros -Q build $< && ./ut 

clean:
	- rm ut
