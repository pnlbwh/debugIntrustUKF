all: 
	source ./env.sh; stack build && stack exec pipeline

clean: 
	rm -f *.out *.err core.*
