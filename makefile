export SHELL=/bin/bash

##  For now, this is just used to test if things can be compiled from source
maketest: maketest.ros
	ros -Q build $^


