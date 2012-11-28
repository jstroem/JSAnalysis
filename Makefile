# Makefile for deleting

make: 
	scalac src/*.scala -deprecation

run:
	scala JSAnalysis -print-ast -graph-ast -graph-cfg -graph-cse test/*.js

clean:
	rm -Rf *.class test/*.ast test/*.gif test/*.dot JSAnalyzer