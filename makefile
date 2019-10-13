export SHELL=/bin/bash

##  For now, this is just used to test if things can be compiled from source
maketest: maketest.ros lisp-linux-sa-utils.lisp
	ros -Q build $<

clean:
	- rm maketest
