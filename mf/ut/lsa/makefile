
LISP_FILES=$(wildcard *.lisp)
LISP_FILES+=$(wildcard usr/*.lisp)
test: ut
	./ut && echo "good!" 

ut: ut.ros $(LISP_FILES)
	ros -Q build $<

clean:
	- rm ut
	- rm *~
