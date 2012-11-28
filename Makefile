# Makefile for deleting

make: compile

run:
	scala JSAnalysis -print-ast -graph-ast -graph-cfg -graph-cse test/*.js

clean:
	rm -Rf *.class test/*.ast test/*.gif test/*.dot JSAnalyzer

compile:
	scalac src/*.scala -deprecation

all: clean compile run
