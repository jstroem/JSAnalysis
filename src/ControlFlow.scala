package JSAnalyzer

object CFG {
	abstract class ControlFlowNode	
	case class ControlFlowASTNode(ASTel:Any = null) extends ControlFlowNode
	case class Merge(label:String) extends ControlFlowNode
	case class Continue(i:AST.Identifier) extends ControlFlowNode
	case class Return() extends ControlFlowNode
	case class Expression(e:AST.Expression) extends ControlFlowNode
	case class Assignments(i:AST.Identifier,e:AST.Expression) extends ControlFlowNode
	case class If(e:AST.Expression) extends ControlFlowNode
	case class ThrowNode(e:AST.Expression) extends ControlFlowNode
	case class EmptyNode() extends ControlFlowNode

	case class ControlFlowGraph(
		start : ControlFlowNode, 
		end: ControlFlowNode, 
		nodes: List[ControlFlowNode] = List(), 
		edges: List[(ControlFlowNode, ControlFlowNode)] = List(), 
		labels:Map[(ControlFlowNode, ControlFlowNode), String] = Map()
	) {
		def +(el: ControlFlowNode ) = ControlFlow.append( this, el )
		def ::(cfg: ControlFlowGraph ) = ControlFlow.concat( this, cfg )

		def >(el: ControlFlowNode) = ControlFlow.prepend( this, el )
	}
}


case class NotImplementedException(s:String="")  extends Exception

object ControlFlow {
	/******************************/
	/******** HELPER METHODS ******/
	/******************************/
	def append( cfg : CFG.ControlFlowGraph, el : CFG.ControlFlowNode,label: Option[String] = None ) : CFG.ControlFlowGraph = {
		label match {
			case Some(label) => CFG.ControlFlowGraph(el,cfg.end,el :: cfg.nodes,(el,cfg.start) :: cfg.edges,cfg.labels ++  Map(((el,cfg.start),label)))
			case None => CFG.ControlFlowGraph(el,cfg.end,el :: cfg.nodes,(el,cfg.start) :: cfg.edges,cfg.labels)
		}	
	}

	def prepend( cfg : CFG.ControlFlowGraph, el: CFG.ControlFlowNode, label:Option[String] = None ) : CFG.ControlFlowGraph = {
		label match {
			case Some(label) => CFG.ControlFlowGraph(cfg.start,el,el :: cfg.nodes,(cfg.end,el) :: cfg.edges, cfg.labels ++ Map(((cfg.end,el),label)))
			case None => CFG.ControlFlowGraph(cfg.start,el,el :: cfg.nodes,(cfg.end,el) :: cfg.edges, cfg.labels)
		}
		
	}

	def concat(cfg1 : CFG.ControlFlowGraph, cfg2: CFG.ControlFlowGraph, label: Option[String] = None ) : CFG.ControlFlowGraph = {
		label match {
			case Some(label) => CFG.ControlFlowGraph(
					cfg1.start,
					cfg2.end,
					cfg1.nodes ::: cfg2.nodes, 
					(cfg1.end,cfg2.start) :: cfg1.edges ::: cfg2.edges, 
					cfg1.labels ++ cfg2.labels ++ Map(((cfg1.end,cfg2.start),label))
				)
			case None => CFG.ControlFlowGraph(
					cfg1.start,
					cfg2.end,
					cfg1.nodes ::: cfg2.nodes, 
					(cfg1.end,cfg2.start) :: cfg1.edges ::: cfg2.edges, 
					cfg1.labels ++ cfg2.labels
				)
		}
	}

	def emptyCFG() : CFG.ControlFlowGraph = {
		var e = CFG.EmptyNode()
		CFG.ControlFlowGraph(e,e,List(e))
	}

	def branchMerge(cfg: CFG.ControlFlowGraph, branches: List[(CFG.ControlFlowGraph,Option[String])], mergePoint: CFG.ControlFlowNode ) : CFG.ControlFlowGraph = {
		if (branches.size == 0) cfg
		else {
			branches.foldLeft(CFG.ControlFlowGraph(cfg.start,cfg.end,mergePoint :: cfg.nodes, cfg.edges, cfg.labels)){
				case (cfg, (added, label)) => label match {
					case Some(label) => CFG.ControlFlowGraph(	
							cfg.start,
							mergePoint, 
							cfg.nodes ::: added.nodes, 
							(cfg.end,added.start) :: (added.end,mergePoint) :: cfg.edges ::: added.edges, 
							cfg.labels ++ added.labels ++ Map(((cfg.end,added.start),label))
						)
					case None => CFG.ControlFlowGraph(	
							cfg.start,
							mergePoint, 
							cfg.nodes ::: added.nodes, 
							(cfg.end,added.start) :: (added.end,mergePoint) :: cfg.edges ::: added.edges, 
							cfg.labels ++ added.labels
						)
				}
			}
		}
	}

	/******************************/
	/*** Recursivly walkthrough ***/
	/******************************/
	def statement( s:AST.Statement ) : CFG.ControlFlowGraph = s match {
		case AST.Block( sl ) => sl match {
			case Some(sl) => statements( sl )
			case None => emptyCFG()
		}
		case AST.VariableStatement(vds) => throw NotImplementedException()
		case AST.EmptyStatement() => throw NotImplementedException()
		case AST.ExpressionStatement(e) => expression(e)
 		case AST.IfStatement(e,s1,os2) => {
			var cfg2 = os2 match {
				case Some(s2) => statement( s2 )
				case None => emptyCFG()
			}
			branchMerge(expression(e) > CFG.If(e), List((statement(s1),Some("True")),(cfg2,Some("False"))), CFG.Merge("If Merge"))
		}
		case AST.WhileStatement(e, s) => throw NotImplementedException()
		case AST.DoWhileStatement() => throw NotImplementedException()
		case AST.ForStatement(_,_,_,_) => throw NotImplementedException()
		case AST.ForInStatement() => throw NotImplementedException()
		case AST.ContinueStatement(oi) => throw NotImplementedException()
		case AST.BreakStatement(i) => throw NotImplementedException()
		case AST.ReturnStatement(oe) => throw NotImplementedException()
		case AST.WithStatement(e,s) => throw NotImplementedException()
		case AST.LabelledStatement(i,s) => throw NotImplementedException()
		case AST.SwitchStatement(e,cb) => throw NotImplementedException()
		case AST.ThrowStatement(e) => throw NotImplementedException()
		case AST.TryStatement(b,oc,of) => throw NotImplementedException()
	}

	def statements(ss : List[AST.Statement] ) : CFG.ControlFlowGraph = {
		ss.foldLeft(emptyCFG)((cfg,add) => cfg :: statement(add))
	}

	def expression( e:AST.Expression ) : CFG.ControlFlowGraph = e match {
		case _ => { //TODO: Should really check if the e has any sideeffects
			var n = CFG.Expression(e)
			CFG.ControlFlowGraph(n,n, List(n))
		}
	}
}