
LISP_FILES=$(wildcard *.lisp)
LISP_FILES+=$(wildcard usr/*.lisp)

ut: ut.ros $(LISP_FILES)
	ros dynamic-space-size=8000  $<

clean:
	- rm ut
	- rm *~
