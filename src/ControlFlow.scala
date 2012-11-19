package JSAnalyzer

import java.util.UUID

/** TODO: 
	* Remove Empty nodes
	* functionDeclaration
	* CaseBlocks should be walked though and if a break is caught this should be updated
	* Continue and breaks needs to be dealt with
**/

object CFG {
	abstract class ControlFlowNode()
	// Each statement has their own type 
	case class Merge(label:String, id: String = UUID.randomUUID().toString()) extends ControlFlowNode()
	case class Continue(i:Option[AST.Identifier], id: String = UUID.randomUUID().toString()) extends ControlFlowNode()
	case class Return(e : Option[AST.Expression], id: String = UUID.randomUUID().toString()) extends ControlFlowNode()
	case class Expression(e:AST.Expression, id: String = UUID.randomUUID().toString()) extends ControlFlowNode()
	case class Assignment(i:AST.Identifier,e:Option[AST.Expression], id: String = UUID.randomUUID().toString()) extends ControlFlowNode()
	case class If(e:AST.Expression, id: String = UUID.randomUUID().toString()) extends ControlFlowNode()
	case class DoWhile(e : AST.Expression, id: String = UUID.randomUUID().toString()) extends ControlFlowNode()
	case class ForIn(e1 : AST.ASTNode, e2 : AST.Expression, id: String = UUID.randomUUID().toString()) extends ControlFlowNode()
	case class With(e : AST.Expression, id: String = UUID.randomUUID().toString()) extends ControlFlowNode()
	case class Switch(e : AST.Expression, id: String = UUID.randomUUID().toString()) extends ControlFlowNode()
	abstract class Case() extends ControlFlowNode()
	case class CaseClause(e : AST.Expression, id: String = UUID.randomUUID().toString()) extends Case
	case class DefaultClause(id: String = UUID.randomUUID().toString()) extends Case
	case class Break(id: String = UUID.randomUUID().toString()) extends ControlFlowNode()
	case class Empty(id: String = UUID.randomUUID().toString()) extends ControlFlowNode()

	case class ControlFlowGraph(
		start : ControlFlowNode, 
		end: ControlFlowNode, 
		nodes: List[ControlFlowNode] = List(), 
		edges: List[(ControlFlowNode, ControlFlowNode)] = List(), 
		labels:Map[(ControlFlowNode, ControlFlowNode), String] = Map()
	) {
		def +(el: ControlFlowNode ) = ControlFlow.append( this, el )
		def ::(cfg: ControlFlowGraph ) = ControlFlow.concat( this, cfg )
		def <(el : ControlFlowNode ) = ControlFlow.addNode(this, el)

		def >(el: ControlFlowNode) = ControlFlow.prepend( this, el )
	}
}


case class NotImplementedException(s:String="")  extends Exception
case class NotSupportedException(s:String="")  extends Exception

object ControlFlow {
	/******************************/
	/******** HELPER METHODS ******/
	/******************************/
	def addNode( cfg: CFG.ControlFlowGraph, el: CFG.ControlFlowNode ) : CFG.ControlFlowGraph = {
		CFG.ControlFlowGraph(cfg.start,cfg.end,el :: cfg.nodes,cfg.edges, cfg.labels)
	}

	def append( cfg : CFG.ControlFlowGraph, el : CFG.ControlFlowNode,label: Option[String] = None ) : CFG.ControlFlowGraph = {
		label match {
			case Some(label) => CFG.ControlFlowGraph(el,cfg.end,el :: cfg.nodes,(el,cfg.start) :: cfg.edges,cfg.labels ++ Map(((el,cfg.start),label)))
			case None => CFG.ControlFlowGraph(el,cfg.end,el :: cfg.nodes,(el,cfg.start) :: cfg.edges,cfg.labels)
		}	
	}

	def prepend( cfg : CFG.ControlFlowGraph, el: CFG.ControlFlowNode, label:Option[String] = None ) : CFG.ControlFlowGraph = {
		label match {
			case Some(label) => CFG.ControlFlowGraph(cfg.start,el,el :: cfg.nodes,(cfg.end,el) :: cfg.edges, cfg.labels ++ Map(((cfg.end,el),label)))
			case None => CFG.ControlFlowGraph(cfg.start,el,el :: cfg.nodes,(cfg.end,el) :: cfg.edges, cfg.labels)
		}	
	}



	def incomingNodes(cfg : CFG.ControlFlowGraph, node : CFG.ControlFlowNode) : List[CFG.ControlFlowNode] = {
		cfg.edges.foldLeft(List() : List[CFG.ControlFlowNode])((list,edge) => {
			var (from,to) = edge;
			if (to == node) from :: list
			else list
		});
	}

	def outgoingNodes(cfg : CFG.ControlFlowGraph, node : CFG.ControlFlowNode) : List[CFG.ControlFlowNode] = {
		cfg.edges.foldLeft(List() : List[CFG.ControlFlowNode])((list,edge) => {
			var (from,to) = edge;
			if (from == node) to :: list
			else list
		});
	}

	def concat(cfg1 : CFG.ControlFlowGraph, cfg2: CFG.ControlFlowGraph, label: Option[String] = None ) : CFG.ControlFlowGraph = {
		label match {
			case Some(label) => CFG.ControlFlowGraph(
					cfg1.start,
					cfg2.end,
					cfg1.nodes ::: cfg2.nodes, 
					(cfg1.end,cfg2.start) :: cfg1.edges ::: cfg2.edges, // (cfg1.end,cfg2.start) :: cfg1.edges ::: cfg2.edges, 
					cfg1.labels ++ cfg2.labels ++ Map(((cfg1.end,cfg2.start),label))
				)
			case None => CFG.ControlFlowGraph(
					cfg1.start,
					cfg2.end,
					cfg1.nodes ::: cfg2.nodes, 
					(cfg1.end,cfg2.start) :: cfg1.edges ::: cfg2.edges, //(cfg1.end,cfg2.start) :: cfg1.edges ::: cfg2.edges, 
					cfg1.labels ++ cfg2.labels
				)
		}
	}

	def emptyCFG() : CFG.ControlFlowGraph = {
		singleCFG(CFG.Empty())
	}

	def singleCFG( n : CFG.ControlFlowNode ) : CFG.ControlFlowGraph = {
		CFG.ControlFlowGraph(n,n,List(n))
	}

	def branchMerge(cfg: CFG.ControlFlowGraph, branches: List[(CFG.ControlFlowGraph,Option[String])], mergePoint: CFG.ControlFlowNode ) : CFG.ControlFlowGraph = {
		if (branches.size == 0) cfg
		else {
			branches.foldLeft(CFG.ControlFlowGraph(cfg.start,cfg.end,mergePoint :: cfg.nodes, cfg.edges, cfg.labels)){
				case (newCfg, (added, label)) => label match {
					case Some(label) => CFG.ControlFlowGraph(	
							cfg.start,
							mergePoint, 
							newCfg.nodes ::: added.nodes, 
							(cfg.end,added.start) :: (added.end,mergePoint) :: newCfg.edges ::: added.edges, 
							newCfg.labels ++ added.labels ++ Map(((cfg.end,added.start),label))
						)
					case None => CFG.ControlFlowGraph(	
							cfg.start,
							mergePoint, 
							newCfg.nodes ::: added.nodes, 
							(cfg.end,added.start) :: (added.end,mergePoint) :: newCfg.edges ::: added.edges, 
							newCfg.labels ++ added.labels
						)
				}
			}
		}
	}

	def makeEdge(cfg : CFG.ControlFlowGraph, from: CFG.ControlFlowNode, to: CFG.ControlFlowNode, label: Option[String] ) : CFG.ControlFlowGraph = {
		label match {
			case Some(label) => CFG.ControlFlowGraph(
				cfg.start,
				cfg.end,
				cfg.nodes,
				(from,to) :: cfg.edges,
				cfg.labels ++ Map(((from,to),label))
			)
			case None => CFG.ControlFlowGraph(
				cfg.start,
				cfg.end,
				cfg.nodes,
				(from,to) :: cfg.edges,
				cfg.labels
			)
		}
	} 

	def convertBreaks(cfg : CFG.ControlFlowGraph, breakToNode : CFG.ControlFlowNode) : CFG.ControlFlowGraph = {
		cfg.nodes.foldLeft(cfg)((cfg,node) => {
			node match  {
				case CFG.Break(_) => {
					//Remove old edges from this node
					var newEdges = cfg.edges.filter((edge) => {
						var (from,to) = edge
						from != node
					})
					CFG.ControlFlowGraph(cfg.start,cfg.end, cfg.nodes,(node,breakToNode) :: newEdges, cfg.labels)
				}
				case _ => cfg
			}
		})
	}

	def removeEmptyNodes(cfg : CFG.ControlFlowGraph ) : CFG.ControlFlowGraph = {
		var newCfg = cfg.nodes.foldLeft(cfg)((cfg,node) => {
			node match {
				case CFG.Empty(_) => {
					//Remove this empty node
					var newNodes = cfg.nodes.filter((n) => n != node)

					//Bind new edges
					var removedEdges = cfg.edges.filter((edge) => {
						var (from,to) = edge
						from != node && node != to
					})

					var appendNewEdges = incomingNodes(cfg,node).foldLeft(removedEdges)((edges,from) => {
						outgoingNodes(cfg,node).foldLeft(edges)((edges,to) => {
							 (from,to) :: edges
						})
					})

					//Bind new labels
					var removedLabels = cfg.labels.filter((pair) => {
						var ((from,to),label) = pair
						from != node && node != to
					})

					var appendNewLabels = incomingNodes(cfg,node).foldLeft(removedLabels)((labels,from) => {
						outgoingNodes(cfg,node).foldLeft(labels)((labels,to) => {
							(cfg.labels.get(node,to),cfg.labels.get(from,node)) match {
								case (None,None) => labels
								case (Some(s),None) => labels + (((from,to), s))
								case (None,Some(s)) => labels + (((from,to), s))
								case (Some(s1),Some(s2)) => labels + (((from,to), s1 + s2))
							}
						})
					})

					CFG.ControlFlowGraph(cfg.start,cfg.end,newNodes,appendNewEdges,appendNewLabels)
				}
				case _ => cfg
			}
		})

		def findNewStart(cfg : CFG.ControlFlowGraph) : CFG.ControlFlowGraph = {
			cfg.start match {
				case CFG.Empty(_) => {
					var edge = cfg.edges.find((edge) => {
						var (from,to) = edge
						from == cfg.start
					})
					edge match {
						case None => cfg //Could not finde any nodes that gives a new one
						case Some((from,to)) => findNewStart(CFG.ControlFlowGraph(to,cfg.end,cfg.nodes,cfg.edges,cfg.labels))
					}
				}
				case _ => cfg
			}
		}

		def findNewEnd(cfg : CFG.ControlFlowGraph) : CFG.ControlFlowGraph = {
			cfg.end match {
				case CFG.Empty(_) => {
					var edge = cfg.edges.find((edge) => {
						var (from,to) = edge
						to == cfg.end
					})
					edge match {
						case None => cfg //Could not finde any nodes that gives a new one
						case Some((from,to)) => findNewEnd(CFG.ControlFlowGraph(cfg.start,from,cfg.nodes,cfg.edges,cfg.labels))
					}
				}
				case _ => cfg
			}
		}

		var newStart = findNewStart(cfg)

		var newEnd = findNewEnd(cfg)

		CFG.ControlFlowGraph(newStart.start,newEnd.end, newCfg.nodes, newCfg.edges, newCfg.labels)
	}

	/******************************/
	/*** Recursivly walkthrough ***/
	/******************************/
	def statement( s:AST.Statement ) : CFG.ControlFlowGraph = s match {
		case AST.Block( sl ) => sl match {
			case Some(sl) => statements( sl )
			case None => emptyCFG()
		}
		case AST.VariableStatement(vds) => {
			vds.foldLeft(emptyCFG())((cfg,vd) => singleCFG(CFG.Assignment(vd.i,vd.a)) :: cfg)
		}
		case AST.EmptyStatement() => throw NotImplementedException()
		case AST.ExpressionStatement(e) => expression(e)
 		case AST.IfStatement(e,s1,os2) => {
			var cfg2 = os2 match {
				case Some(s2) => statement( s2 )
				case None => emptyCFG()
			}
			branchMerge(singleCFG(CFG.If(e)), List((statement(s1),Some("True")),(cfg2,Some("False"))), CFG.Merge("If"))
		}
		case AST.WhileStatement(e, s) => {
			/* This works but as discused in lection 12 it can be made as a do while which is easier to optimize
			var check = CFG.While(e)
			var stmt = statement(s)
			var cfg = append(statement(s),check,Some("True"))
			var end = CFG.Merge("While")
			makeEdge(makeEdge(cfg,cfg.end,cfg.start,Some("Loop")) > end,check,end,Some("False"))*/
			statement( AST.IfStatement(e, AST.DoWhileStatement(e,s), None ) )
		}
		case AST.DoWhileStatement(e,s) => {
			var cfg = (statement(s) > CFG.DoWhile(e)) + CFG.Merge("While")
			makeEdge(cfg,cfg.end,cfg.start,Some("Loop"))
		}
		case AST.ForStatement(oe1,oe2,oe3,s) => {
			// Converts this into a while statement 
			var init = oe1 match {
				case Some(e) => e match {
					case e : AST.Expression => AST.ExpressionStatement(e)
					case e : AST.Statement => e
					case _ => throw NotImplementedException("This is regarding issue #1 at github")
				}
				case None => AST.EmptyStatement()
			}
			var check = oe2 match {
				case Some(e) => e
				case None => AST.BooleanLiteral(true)
			}
			var inc = oe3 match {
				case Some(e) => AST.ExpressionStatement(e)
				case None => AST.EmptyStatement()
			}
			statements( List(init, AST.WhileStatement(check, AST.Block( Some(List( s, inc ) )) ) ) )
		}
		case AST.ForInStatement(e1,e2,s) => {
			var check = CFG.ForIn(e1,e2)
			var merge = CFG.Merge("ForIn")
			var stmt = statement(s)
			var cfg = (stmt :: singleCFG(check)) < merge
			makeEdge(makeEdge(CFG.ControlFlowGraph(cfg.start,merge,cfg.nodes,cfg.edges,cfg.labels),check,merge, Some("False")),stmt.end,check,Some("Loop"))
		}
		case AST.LabelledStatement(i,s) => throw NotImplementedException()
		case AST.SwitchStatement(e,cb) => {
			var endNode = CFG.Merge("Switch")
			var cfg = singleCFG(CFG.Switch(e)) < endNode
			caseBlock(cb, cfg, endNode)
		}
			
		case AST.TryStatement(b,oc,of) => {
			throw NotSupportedException("Try Statements is not support (Regarding issue #2 at Github)")
			/** This might work but because of the lack of time/info this is unsupported for now
			b.sl match {
				case Some(ss) => (oc,of) match {
					case (None,None) => statements(ss)
					case (None,Some(b)) => b.sl match {
						case None => statements(ss)
						case Some(ss2) => statements(ss2) :: statements(ss)
					}
					case (Some(c),None) => catchBlock(c, statements(ss))
					case (Some(c),Some(b)) => b.sl match {
						case None => catchBlock(c, statements(ss))
						case Some(ss2) => catchBlock(c, statements(ss2) :: statements(ss))
					}
				}
				case None => of match {
					case Some(b) => b.sl match {
						case Some(ss) => statements(ss)
						case None => emptyCFG()
					}
					case None => emptyCFG()
				}
			} **/
		}
		case AST.ContinueStatement(oi) => singleCFG(CFG.Continue(oi))
		case AST.BreakStatement(i) => singleCFG(CFG.Break())
		case AST.ReturnStatement(oe) => singleCFG(CFG.Return(oe))
		case AST.ThrowStatement(e) => {
			throw NotSupportedException("Try Statements is not support (Regarding issue #2 at Github)")
			/** This might work but because of the lack of time/info this is unsupported for now
			singleCFG(CFG.Throw(e))
			**/
		}
		case AST.WithStatement(e,s) => statement(s) + CFG.With(e)
	}

	def statements(ss : List[AST.Statement] ) : CFG.ControlFlowGraph = {
		ss.foldLeft(emptyCFG)((cfg,add) => statement(add) :: cfg)
	}

/*
	def catchBlock(c : AST.Catch, cfg : CFG.ControlFlowGraph ) : CFG.ControlFlowGraph = {
		c.b.sl match {
			case None => cfg < CFG.Catch(c.i)
			case Some(ss) => throw NotImplementedException("Catch block not impl.")
		}
	} 
*/

	def caseBlock(cb : AST.CaseBlock, switch : CFG.ControlFlowGraph, endNode : CFG.ControlFlowNode) : CFG.ControlFlowGraph = {
		// TODO Should go through each statment and remove breaks
		var (cfg,ccs,defNode) = cb.ccs.foldLeft((emptyCFG(),List() : List[CFG.ControlFlowNode],false))((res,cc) => {
			var (cfg,ccs,defNode) = res
			cc match {
				case AST.CaseClause(e,sso) => {
					sso match {
						case Some(ss) => {
							var caseClause = CFG.CaseClause(e)
							((statements(ss) + caseClause)  :: cfg, caseClause :: ccs,defNode)
						}
						case None => {
							var caseClause = CFG.CaseClause(e)
							(cfg > caseClause, caseClause :: ccs,defNode)
						}
					}
				}
				case AST.DefaultClause(sso) => {
					sso match {
						case Some(ss) => {
							var caseClause = CFG.DefaultClause()
							((statements(ss) + caseClause)  :: cfg, caseClause :: ccs,true)
						}
						case None => {
							var caseClause = CFG.DefaultClause()
							(cfg > caseClause, caseClause :: ccs,true)
						}
					}
				}
			}
		})

		var cfgBreak = convertBreaks(cfg, endNode)


		//Appending the endNode with the begining of the statement in case of no cases match and no default node.
		var cfgEnd = if (defNode) cfgBreak > endNode else makeEdge(cfgBreak > endNode,switch.end, endNode, None)

		ccs.foldLeft(CFG.ControlFlowGraph(switch.start,cfgEnd.end, switch.nodes ::: cfgEnd.nodes, switch.edges ::: cfgEnd.edges, switch.labels ++ cfgEnd.labels))((cfg,cc) => {
			makeEdge(cfg,switch.start,cc,None)
		})
	}

	def expression( e:AST.Expression ) : CFG.ControlFlowGraph = singleCFG(CFG.Expression(e))

	def functionDeclaration( fd : AST.FunctionDeclaration ) : CFG.ControlFlowGraph = {
		emptyCFG() //TODO
	}

	def sourceElements( ses : List[AST.SourceElement] ) : CFG.ControlFlowGraph = {
		ses.foldLeft(emptyCFG)((cfg,se) => sourceElement(se) :: cfg)
	}
	
	def sourceElement( se : AST.SourceElement ) : CFG.ControlFlowGraph = {
		se match {
			case se : AST.Statement => statement( se )
			case se : AST.FunctionDeclaration => functionDeclaration( se )
		}
	}

	def program( p : AST.Program ) : CFG.ControlFlowGraph = p.a match {
		case Some(se) => removeEmptyNodes(sourceElements(se))
		case None => emptyCFG()
	}
}