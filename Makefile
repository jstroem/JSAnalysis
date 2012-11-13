# Makefile for deleting

make: 
	scalac src/*.scala

run:
	scala JSAnalysis -print-ast -graph-ast -graph-cfg test/*.js

clean:
	rm -Rf src/*.class test/*.ast test/*.gif test/*.dot src/JSAnalyzer