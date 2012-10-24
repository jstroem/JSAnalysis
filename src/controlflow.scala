abstract class ControlFlowNode

case class ControlFlowASTNode(ASTel:Any = null) extends ControlFlowNode
case class MergeNode(label:String) extends ControlFlowNode

class ControlFlowGraph(startNode : Option[ControlFlowNode] = None, endNode: Option[ControlFlowNode] = None, n: List[ControlFlowNode] = List(), e: Map[ControlFlowNode, ControlFlowNode] = Map(), l:Map[(ControlFlowNode, ControlFlowNode), String] = Map()) {
	var start: Option[ControlFlowNode] = startNode
	var end: Option[ControlFlowNode] = endNode
	var nodes: List[ControlFlowNode] = n
	var edges: Map[ControlFlowNode, ControlFlowNode] = e
	var labels: Map[(ControlFlowNode, ControlFlowNode), String] = l

	def append( el : ControlFlowNode,label: String = null ) : ControlFlowGraph = {
		this.start match {
			case None => { //Assume everything is empty
				println(this.nodes.toString())
				new ControlFlowGraph(Some(el),Some(el),el :: this.nodes,this.edges,this.labels)
			}
			case Some(n) => {
				if (label != null){
					new ControlFlowGraph(Some(el),this.end,el :: this.nodes,this.edges ++ Map((el,n)) ,this.labels ++ Map(((el,n),label)))
				} else {
					new ControlFlowGraph(Some(el),this.end,el :: this.nodes,this.edges ++ Map((el,n)),this.labels)
				}
			}
		}
	}

	def prepend( el: ControlFlowNode, label:String = null ) : ControlFlowGraph = {
		this.end match {
			case None => { //Assume everything is empty
				new ControlFlowGraph(Some(el),Some(el),el :: this.nodes,this.edges,this.labels)
			}
			case Some(n) => {
				if (label != null)
					new ControlFlowGraph(this.start,Some(el),el :: this.nodes,this.edges ++ Map((n,el)),this.labels ++ Map(((n,el),label)))
				else
					new ControlFlowGraph(this.start,Some(el),el :: this.nodes,this.edges ++ Map((n,el)),this.labels)
			}
		}
	}

	def concat(cfg: ControlFlowGraph, label: String = null ) : ControlFlowGraph = {
		(this.start,cfg.start,this.end) match {
			case (None,None,_) => cfg//Both empty so just reply one of them.
			case (None,Some(n),_) => cfg //This is empty
			case (Some(n),None,_) => this //cfg is empty
			case (Some(n),Some(m),Some(k)) => {
				if (label != null)
					new ControlFlowGraph(this.start,cfg.end,this.nodes ::: cfg.nodes, this.edges ++ cfg.edges ++ Map((k,m)), this.labels ++ cfg.labels ++ Map(((k,m),label)))
				else 
					new ControlFlowGraph(this.start,cfg.end,this.nodes ::: cfg.nodes, this.edges ++ cfg.edges ++ Map((k,m)), this.labels ++ cfg.labels)
			}
		}
	}

	
}


object ControlFlow {
	//going recursively

	def statement( s:AST.Statement ) : ControlFlowGraph = s match {
		case AST.Block( sl ) => statements( sl )
		case AST.IfStatement(e,s1,s2) => {
			var cfg1 = expression( e ) 
			cfg1
/*
			var cfg2 = cfg1.branch( statement( s1 ) )
			var cfg3 = cfg1.branch( statement( s2 ) )*/
		}
		case _ => new ControlFlowGraph()
	}

	def statements(ss : List[AST.Statement] ) : ControlFlowGraph = ss match {
		case s :: ss => statement( s ).concat( statements( ss ) )
		case Nil => new ControlFlowGraph()
	}

	def expression( e:AST.Expression ) : ControlFlowGraph = new ControlFlowGraph()
}