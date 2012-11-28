package JSAnalyzer

object CSE {
	def extractIdentifiers(cfg : CFG.ControlFlowGraph) : List[AST.Identifier] = {
		cfg.nodes.foldLeft(List[AST.Identifier]())((list,node) => node match {
			case CFG.Assignment (i,e,_) => i::list; 
			case _ => list;
		})
	}
	
	def extractExpressionAssignments(cfg : CFG.ControlFlowGraph) : List[AST.Expression] = {
		def expressionType(exp : AST.Expression, expList : List[AST.Expression]) : List[AST.Expression] = {
			exp match {
			case AST.ExpressionList (es) => es.foldLeft(expList)((list,exp) => expressionType(exp,list))
			case AST.AssignmentExpression (e1,op,e2) => println("ASSIGNEXP" + e1 + op + "(" + e2 + ")"); 
														e2 match {
															case AST.AssignmentExpression (_,_,_) => 																
																expressionType(e2, expList)
															case _ => 
																expressionType(e2, e2::expList)
														}
			case AST.BinaryExpression (op,e1,e2) => expressionType(e2, expressionType (e1,expList))
			case AST.UnaryExpression (op, e) => expressionType (e,expList)
			case AST.PostfixExpression (op,e) => expressionType (e,expList)
			case AST.ConditionalExpression (e1,e2,e3) => expressionType(e3,expressionType(e2, expressionType (e1,expList)))
			case AST.CallExpression (e,oes) => expressionType(e, oes match {
																	case Some (es) => es.foldLeft(expList)((list,exp) => expressionType(exp,list))
																	case None => expList})	
			case AST.AllocationExpression (e) => expressionType (e,expList)
			case AST.ArrayAccessExpression (e1,e2) => expressionType(e2, expressionType (e1,expList))
			case AST.ObjectAccessExpression (e,i) => expressionType (e,expList)
			case AST.ArrayLiteral (oes) =>  oes match {
											case Some (es) => es.foldLeft(expList)((list,exp) => expressionType(exp,list))
											case None => expList}
			case _ => expList	
			}
		}
		
		cfg.nodes.foldLeft(List[AST.Expression]())((list,node) => node match {
			case CFG.Continue(oe,_) => oe match {
										case Some (e) => expressionType(e,list)
										case None => list}
			case CFG.Return (oe,_) => oe match {
										case Some (e) => expressionType(e,list)
										case None => list}
			case CFG.Expression (e,_) => expressionType(e, list)
			case CFG.Assignment (i,e,_) => e match {
												case Some (e) => e match {
																	case AST.AssignmentExpression (_,_,_) => expressionType(e,list)
																	case _ => e::list}
												case None => list}
			case CFG.If (e,_) => expressionType(e,list)
			case CFG.DoWhile (e,_)=> expressionType(e,list)
			case CFG.ForIn(e1,e2,_) => e1 match {
					case e1 : AST.Expression => expressionType(e2, expressionType (e1,list))
					case _ => expressionType (e2,list)
			}
			case CFG.With (e,_) => expressionType(e,list)
			case CFG.Switch (e,_) => expressionType(e,list)
			case CFG.CaseClause (e,_) => expressionType(e,list)
			case _ => list;
		})
	}	
	
	def getCSELatticeBottom(cfg : CFG.ControlFlowGraph) : Map[AST.Identifier, List[AST.Expression]] = {
		var idens : List[AST.Identifier] = extractIdentifiers(cfg)
		var exps : List[AST.Expression] = extractExpressionAssignments(cfg).distinct
		idens.foldLeft(Map() : Map[AST.Identifier, List[AST.Expression]])((map,identifier) => map+(identifier -> exps))
	}
	
	def getCSELatticeTop(cfg : CFG.ControlFlowGraph) : Map[AST.Identifier, List[AST.Expression]] = {
		var idens : List[AST.Identifier] = extractIdentifiers(cfg)
		idens.foldLeft(Map() : Map[AST.Identifier, List[AST.Expression]])((map,identifier) => map+(identifier -> List()))
	}
	
}


object CSEGrapher {
	def nodeToString(n : CFG.ControlFlowNode) : String = n match {
 		case CFG.Empty(_) => "Empty"
 		case CFG.Break(_) => "Break"
 		case CFG.Merge(label,_) => "Merge"
 		case CFG.Continue(olabel,_) => "Continue: %s".format(olabel.getOrElse("(no label)"))
 		case CFG.Return(oe,_) => "Return: %s".format(oe.getOrElse("(no expression)"))
 		case CFG.Expression(e,_) => "Expression: %s".format(e)
 		case CFG.Assignment(i,oe,_) => "Assignment "+i+" = %s".format(oe.getOrElse("(no expression)"))
 		case CFG.If(e,_) => "If: %s".format(e)
 		case CFG.DoWhile(e,_) => "While: %s".format(e)
 		case CFG.ForIn(e1, e2,_) => "ForIn %s in %s".format(e1,e2)
 		case CFG.With(e,_) => "With: %s".format(e)
 		case CFG.Switch(e,_) => "Switch: %s".format(e)
 		case CFG.CaseClause(e,_) => "Case: %s".format(e)
 		case CFG.DefaultClause(_) => "Default Case"
 	}

	 def nodeId( n : CFG.ControlFlowNode ) : String = n match {
	 	case CFG.Empty(id) => id
 		case CFG.Break(id) => id
 		case CFG.Merge(_,id) => id 
 		case CFG.Continue(_,id) => id 
 		case CFG.Return(_,id) => id 
 		case CFG.Expression(_, id) => id
 		case CFG.Assignment(_,_,id) => id 
 		case CFG.If(_, id) => id 
 		case CFG.DoWhile(_, id) => id 
 		case CFG.ForIn(_, _, id) => id
 		case CFG.With(_,id) => id 
 		case CFG.Switch(_,id) => id 
 		case CFG.CaseClause(_,id) => id
 		case CFG.DefaultClause(id) => id
	 }

	def graph(cfg : CFG.ControlFlowGraph, cseMap : (Map[(CFG.ControlFlowNode, CFG.ControlFlowNode), Map[AST.Identifier, List[AST.Expression]]])) : GraphvizDrawer.Graph = {
		new GraphvizDrawer.Graph {
			var start = GraphvizDrawer.Node("start", "Start", Some(GraphvizDrawer.Diamond()))
			var end = GraphvizDrawer.Node("end", "End", Some(GraphvizDrawer.Square()))

			def nodes() = {
				cfg.nodes.foldLeft(List(start,end))((list,node) => {
					GraphvizDrawer.Node(nodeId( node ), GraphvizDrawer.escape( nodeToString( node ) ) ) :: list
				})
			}
			def edges() = {
			 	var edge1 = GraphvizDrawer.Edge("start",nodeId(cfg.start))
			 	var edge2 = GraphvizDrawer.Edge(nodeId(cfg.end),"end")
				cfg.edges.foldLeft(List(edge1,edge2))((list,edge) => {
					var (from,to) = edge
					GraphvizDrawer.Edge(nodeId(from),nodeId(to), Option (cseElemToString(cseMap.get(from,to)))) :: list
				})
			}
			
			def name() = "ControlFlowGraph"
		}
	}
	
	def cseElemToString(elem : Option[Map[AST.Identifier, List[AST.Expression]]]) : String = {
		def printList(list : List[AST.Expression], string : String) : String = {
			list.size match {
				case 0 => string
				case _ => list.head.toString
			}
		}
		
		elem match {
			case Some (e) => e.foldLeft(""){ case (string,(key,value)) => GraphvizDrawer.escape(string + key + " => " + printList(value, "") + "\n")}
			case None => "" // not used
		}
	}
}