all: 
	source ./env.sh; stack build && stack exec pipeline

clean: 
	rm *.out *.err core.*
