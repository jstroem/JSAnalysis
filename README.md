JSAnalysis
==========

To get graph drawnings you need to install graphviz including dot.

#Run
Use the following command to run:

	scala JSAnalysis
		[-print-ast (print of the ast)]
		[-graph-ast (Graph of the ast)]
		[-graph-cfg (Graph of the cfg)]
		files/folders..

An example could be
	
	scala JSAnalysis -print-ast -graph-ast ../test/*.js

#TODO
Todolist in each part

##CFG
* Check following works:
	* ForIn
	* Switch

* Remove Empty nodes
* functionDeclaration
* CaseBlocks should be walked though and if a break is caught this should be updated
* Continue needs to be dealt with