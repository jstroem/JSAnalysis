package JSAnalyzer

object GraphvizExporter {
	var line = "\n"

 	def export(graphName: String, cfg: CFG.ControlFlowGraph, export: java.io.PrintStream = System.out) = {
 		export.println("digraph "+graphName+" {")
 		drawGraph( cfg, export )
 		export.println("}")
 	}

 	def drawGraph(cfg : CFG.ControlFlowGraph, export : java.io.PrintStream = System.out ) {
 		export.println("\tstart [shape=Mdiamond];")
		export.println("\tend [shape=Msquare];")

 		cfg.nodes.foreach((node) => export.println("\t"+nodeToString(node)))

 		export.println("\tstart [shape=Mdiamond];")
		export.println("\tend [shape=Msquare];")

		export.println("\tstart -> \""+nodeToId(cfg.start)+"\";")
		export.println("\t\""+nodeToId(cfg.end)+"\" -> end;")

 		cfg.edges.foreach{case (n1,n2) => export.println("\t"+edgeToString(n1,n2,cfg.labels.get(n1,n2)))}
 	}

 	def nodeToString( n : CFG.ControlFlowNode ) : String = n match {
 		case CFG.Empty(id) => "\"%s\" [shape=record label=\"%s\"];".format(id, "{Empty}")
 		case CFG.Break(id) => "\"%s\" [shape=record label=\"%s\"];".format(id, "{Break}")
 		case CFG.Merge(label,id) => "\"%s\" [shape=record label=\"%s\"];".format(id, "{Merge: "+label+"}")
 		case CFG.Continue(olabel,id) => olabel match {
 			case None => "\"%s\" [shape=record label=\"%s\"];".format(id, "{Continue: (No label)}")
 			case Some(label) => "\"%s\" [shape=record label=\"%s\"];".format(id, "{Continue: "+label+"}")
 		}
 		case CFG.Return(oe,id) => oe match {
 			case None => "\"%s\" [shape=record label=\"%s\"];".format(id, "{Return: (No expression)}")
 			case Some(e) => "\"%s\" [shape=record label=\"%s\"];".format(id, "{Return: "+expToString(e)+"}")
 		}
 		case CFG.Expression(e, id) => "\"%s\" [shape=record label=\"%s\"];".format(id, "{Expression: "+expToString(e)+"}")
 		case CFG.Assignment(i,oe,id) => oe match {
 			case None => "\"%s\" [shape=record label=\"%s\"];".format(id, "{Assignment: "+i+" = (no expression)}")
 			case Some(e) => "\"%s\" [shape=record label=\"%s\"];".format(id, "{Assignment: "+i+" = "+expToString(e)+"}")
 		}
 		case CFG.If(e, id) => "\"%s\" [shape=record label=\"%s\"];".format(id, "{If: "+expToString(e)+"}")
 		case CFG.While(e, id) => "\"%s\" [shape=record label=\"%s\"];".format(id, "{While: "+expToString(e)+"}")
 		case CFG.ForIn(e1, e2, id) => "\"%s\" [shape=record label=\"%s\"];".format(id, "{ForIn: "+expToString(e1)+" in "+expToString(e2)+"}")
 		case CFG.Throw(e,id) => "\"%s\" [shape=record label=\"%s\"];".format(id, "{Throw: "+expToString(e)+"}")
 		case CFG.With(e,id) => "\"%s\" [shape=record label=\"%s\"];".format(id, "{With: "+expToString(e)+"}")
 		case CFG.Switch(e,id) => "\"%s\" [shape=record label=\"%s\"];".format(id, "{Switch: "+expToString(e)+"}")
 		case CFG.Catch(i,id) => "\"%s\" [shape=record label=\"%s\"];".format(id, "{Catch: "+i+"}")
 		case CFG.CaseClause(e,id) => "\"%s\" [shape=record label=\"%s\"];".format(id, "{Case: "+expToString(e)+"}")
 		case CFG.DefaultClause(id) => "\"%s\" [shape=record label=\"%s\"];".format(id, "{Default Case}")
	 }

	 def expToString( e : AST.ASTNode ) : String = {
	 	e.toString().map(_ match { 
          case '\'' => "&apos;"
          case '"' => "&quot;"
          case '<' => "&lt;"
          case '>' => "&gt;"
          case other => other toString
        }) mkString;
	 }

	 def nodeToId( n : CFG.ControlFlowNode ) : String = n match {
	 	case CFG.Empty(id) => id
 		case CFG.Break(id) => id
 		case CFG.Merge(_,id) => id 
 		case CFG.Continue(_,id) => id 
 		case CFG.Return(_,id) => id 
 		case CFG.Expression(_, id) => id
 		case CFG.Assignment(_,_,id) => id 
 		case CFG.If(_, id) => id 
 		case CFG.While(_, id) => id 
 		case CFG.ForIn(_, _, id) => id
 		case CFG.Throw(_,id) => id 
 		case CFG.With(_,id) => id 
 		case CFG.Switch(_,id) => id
 		case CFG.Catch(_,id) => id 
 		case CFG.CaseClause(_,id) => id
 		case CFG.DefaultClause(id) => id
	 }

 	def edgeToString(n1 : CFG.ControlFlowNode, n2 : CFG.ControlFlowNode, label : Option[String]) : String = label match {
 		case Some(label) => "\"%s\" -> \"%s\" [label=\"%s\"];".format(nodeToId(n1),nodeToId(n2),label)
 		case None => "\"%s\" -> \"%s\";".format(nodeToId(n1),nodeToId(n2))
 	}
}