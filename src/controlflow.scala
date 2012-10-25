
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

	case class ControlFlowGraph(
		start : Option[ControlFlowNode] = None, 
		end: Option[ControlFlowNode] = None, 
		nodes: List[ControlFlowNode] = List(), 
		edges: Map[ControlFlowNode, ControlFlowNode] = Map(), 
		labels:Map[(ControlFlowNode, ControlFlowNode), String] = Map()
	) {
		def +(el: ControlFlowNode ) = ControlFlow.append( this, el )
		def ::(cfg: ControlFlowGraph ) = ControlFlow.concat( this, cfg )
	}
}


case class NotImplementedException(s:String="")  extends Exception

object ControlFlow {
	/******************************/
	/******** HELPER METHODS ******/
	/******************************/
	def append( cfg : CFG.ControlFlowGraph, el : CFG.ControlFlowNode,label: String = null ) : CFG.ControlFlowGraph = {
		cfg.start match {
			case None => { //Assume everything is empty
				CFG.ControlFlowGraph(Some(el),Some(el),el :: cfg.nodes,cfg.edges,cfg.labels)
			}
			case Some(n) => {
				if (label != null){
					CFG.ControlFlowGraph(Some(el),cfg.end,el :: cfg.nodes,cfg.edges ++ Map((el,n)) ,cfg.labels ++ Map(((el,n),label)))
				} else {
					CFG.ControlFlowGraph(Some(el),cfg.end,el :: cfg.nodes,cfg.edges ++ Map((el,n)),cfg.labels)
				}
			}
		}
	}

	def prepend( cfg : CFG.ControlFlowGraph, el: CFG.ControlFlowNode, label:String = null ) : CFG.ControlFlowGraph = {
		cfg.end match {
			case None => { //Assume everything is empty
				CFG.ControlFlowGraph(Some(el),Some(el),el :: cfg.nodes,cfg.edges,cfg.labels)
			}
			case Some(n) => {
				if (label != null)
					CFG.ControlFlowGraph(cfg.start,Some(el),el :: cfg.nodes,cfg.edges ++ Map((n,el)),cfg.labels ++ Map(((n,el),label)))
				else
					CFG.ControlFlowGraph(cfg.start,Some(el),el :: cfg.nodes,cfg.edges ++ Map((n,el)),cfg.labels)
			}
		}
	}

	def concat(cfg1 : CFG.ControlFlowGraph, cfg2: CFG.ControlFlowGraph, label: String = null ) : CFG.ControlFlowGraph = {
		(cfg1.start,cfg2.start,cfg1.end) match {
			case (None,None,_) => cfg1//Both empty so just reply one of them.
			case (None,Some(n),_) => cfg2 //cfg1 is empty
			case (Some(n),None,_) => cfg1 //cfg2 is empty
			case (Some(n),Some(m),Some(k)) => {
				if (label != null)
					CFG.ControlFlowGraph(cfg1.start,cfg2.end,cfg1.nodes ::: cfg2.nodes, cfg1.edges ++ cfg2.edges ++ Map((k,m)), cfg1.labels ++ cfg2.labels ++ Map(((k,m),label)))
				else 
					CFG.ControlFlowGraph(cfg1.start,cfg2.end,cfg1.nodes ::: cfg2.nodes, cfg1.edges ++ cfg2.edges ++ Map((k,m)), cfg1.labels ++ cfg2.labels)
			}
		}
	}

	/******************************/
	/*** Recursivly walkthrough ***/
	/******************************/
	def statement( s:AST.Statement ) : CFG.ControlFlowGraph = s match {
		case AST.Block( sl ) => statements( sl )
		case AST.VariableStatement(vds) => throw NotImplementedException()
		case AST.EmptyStatement() => throw NotImplementedException()
		case AST.ExpressionStatement(e) => expression(e)
 		case AST.IfStatement(e,s1,os2) => {
			var cfg1 = expression( e ) :: statement( s1 )
			var cfg2 = os2 match {
				case Some(s2) => expression( e ) :: statement( s2 )
				case None => expression( e )
			}
			cfg1
/*
			var cfg2 = cfg1.branch( statement( s1 ) )
			var cfg3 = cfg1.branch( statement( s2 ) )*/
		}
		case AST.WhileStatement(e:Expression, s:Statement) => throw NotImplementedException()
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

	def statements(ss : List[AST.Statement] ) : CFG.ControlFlowGraph = ss match {
		case s :: ss => statement( s ) :: statements( ss )
		case Nil => CFG.ControlFlowGraph()
	}

	def expression( e:AST.Expression ) : CFG.ControlFlowGraph = e match {
		case _ => { //TODO: Should really check if the e has any sideeffects
			var n = CFG.Expression(e)
			CFG.ControlFlowGraph(Some(n),Some(n), List(n))
		}
	}
}