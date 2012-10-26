package JSAnalyzer

object GraphvizExporter {
	var line = "\n"

 	def export(graphName: String, cfg: CFG.ControlFlowGraph, export: java.io.PrintStream = System.out) = {
 		export.println("digraph "+graphName+" {")
 		walkGraphStartingFrom( cfg.start, cfg, export )
 		export.println("}")
 	}

 	def walkGraphStartingFrom( n : CFG.ControlFlowNode, cfg : CFG.ControlFlowGraph, export : java.io.PrintStream = System.out ) : List[CFG.ControlFlowNode] = {
 		def recPrinter(currNode : CFG.ControlFlowNode, seen : List[CFG.ControlFlowNode]) : List[CFG.ControlFlowNode] = {
 			cfg.edges.foldLeft(seen){
	 			case(seen,(n1 : CFG.ControlFlowNode ,n2 : CFG.ControlFlowNode)) => {
			 		if (currNode == n1) {
			 			export.println(edgeToString(n,n2,cfg.labels.get(n,n2)))
			 			if (!seen.contains(n2)) recPrinter(n2,n2 :: seen)
			 			else seen
			 		}
	 				else seen
	 			}
 			}
 		}
 		recPrinter(n,List(n))
 	}

 	def nodeToString( n : CFG.ControlFlowNode ) : String = n match {
 		case CFG.EmptyNode() => "EmptyNode"
 		case CFG.Return() => "Return"
 		case CFG.Merge(l) => "Merge: "+ l
 		case CFG.Continue(i) => "Continue: " + i
 		case CFG.If(e) => "If: " + e
 		case _ => "TODO" // TODO: Not written yet
 	}

 	def edgeToString(n1 : CFG.ControlFlowNode, n2 : CFG.ControlFlowNode, label : Option[String]) : String = label match {
 		case Some(label) => nodeToString(n1) + " -> " + nodeToString(n2) + "[label="+label+"]"
 		case None => nodeToString(n1) + " -> " + nodeToString(n2)
 	}
}