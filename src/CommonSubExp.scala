package JSAnalyzer

object CommonSubExp{

	class CommonSubExpAnalysis(bottom : Map[AST.Identifier, List[AST.Expression]], top: Map[AST.Identifier, List[AST.Expression]]) extends DataFlowAnalysis.DataFlowAnalysis[Map[AST.Identifier, List[AST.Expression]]] {
		def getBottom : Map[AST.Identifier, List[AST.Expression]] = {
			bottom;
		}

		def getTop : Map[AST.Identifier, List[AST.Expression]] = {
			top;
		}

		def compareElements(m1:Map[AST.Identifier, List[AST.Expression]],m2:Map[AST.Identifier, List[AST.Expression]]) : Option[Boolean] = {
			if(m2.foldLeft(true){case (bool,(key,value)) => var m1values = m1.apply(key);
															bool && 
															value.foldLeft(true)((bool,elem) => m1values.contains(elem) && bool)}){
				Some (true)
			}else{			
				if(m1.foldLeft(true){case (bool,(key,value)) => var m2values = m2.apply(key);
																bool && 
																value.foldLeft(true)((bool,elem) => m2values.contains(elem) && bool)}){
					Some (false)
				}else{
					None
				}
			}
		}

		def getLub(m1:Map[AST.Identifier, List[AST.Expression]], m2:Map[AST.Identifier, List[AST.Expression]]) : Map[AST.Identifier, List[AST.Expression]] = {
			m1.foldLeft(Map():Map[AST.Identifier, List[AST.Expression]]){case (map,(key,value)) => map+(key -> value.intersect(m2.apply(key)))}
		}


		def getGlb(m1:Map[AST.Identifier, List[AST.Expression]], m2:Map[AST.Identifier, List[AST.Expression]]) : Map[AST.Identifier, List[AST.Expression]] = {
			m1.foldLeft(Map():Map[AST.Identifier, List[AST.Expression]]){case (map,(key,value)) => map+(key -> value.union(m2.apply(key)))}																	
		}
		
		def globalFlowFunction(node:CFG.ControlFlowNode, info_in1 : List[Map[AST.Identifier, List[AST.Expression]]], lattice : DataFlowAnalysis.DataFlowAnalysis[Map[AST.Identifier, List[AST.Expression]]])= {		
			var info_in = if(info_in1.isEmpty){lattice.getTop::info_in1}else{info_in1}
			
			def expressionType(exp : AST.Expression, infoMap : Map[AST.Identifier, List[AST.Expression]], identifiers : List[AST.Identifier]) : Map[AST.Identifier, List[AST.Expression]] = {
				exp match {
					case AST.ExpressionList (es) => es.foldLeft(infoMap)((map,e) => expressionType(e,infoMap, identifiers))
					case AST.AssignmentExpression (e1,op,e2) => e1 match{
																	case e1 : AST.Identifier => e2 match {															
																									case AST.AssignmentExpression (_,_,_) => expressionType(e2,infoMap,e1::identifiers)         
																									case _ => var updatedIdentifiers = e1::identifiers
																											var updatedMap = updatedIdentifiers.foldLeft(infoMap)((map,iden) => map+(iden -> List(e2)))
																											updatedIdentifiers.foldLeft(updatedMap)((map, iden) =>
																																		map.foldLeft(map){case (map1,(key,explist)) => 
																																									map1+(key -> explist.filter(exp => !isIdentifierInExpression(exp,iden)))})}	
																	case _	=> infoMap} //The "_" case should not be used
					case AST.BinaryExpression (op,e1,e2) => expressionType(e2, expressionType (e1,infoMap, identifiers), identifiers)
					case AST.UnaryExpression (op, e) => expressionType(e,infoMap, identifiers)							
					case AST.PostfixExpression (op,e) => expressionType(e,infoMap, identifiers)					
					case AST.ConditionalExpression (e1,e2,e3) => expressionType(e3,expressionType(e2, expressionType (e1,infoMap, identifiers), identifiers), identifiers)
					case AST.CallExpression (e,oes) => expressionType(e, oes match {
																			case Some (es) => es.foldLeft(infoMap)((map,e) => expressionType(e,infoMap, identifiers))
																			case None => infoMap}, identifiers)	
					case AST.AllocationExpression (e) => expressionType(e,infoMap, identifiers)
					case AST.ArrayAccessExpression (e1,e2) => expressionType(e2, expressionType (e1,infoMap, identifiers), identifiers)
					case AST.ObjectAccessExpression (e,i) => expressionType(e,infoMap, identifiers)
					case AST.ArrayLiteral (oes) => oes match {
													case Some (es) => es.foldLeft(infoMap)((map,e) => expressionType(e,infoMap, identifiers))
													case None => infoMap}
					case _ => infoMap	
				}
			}
			
			node match {
				case CFG.Merge(_,_) => info_in.foldLeft(lattice.getBottom)((map, e) => lattice.getLub(map,e))
				case CFG.Continue(oe,_) => oe match {				
												case Some (e) => expressionType(e,info_in.head, List())
												case None => info_in.head}
				case CFG.Return(oe,_) => oe match {				
												case Some (e) => expressionType(e,info_in.head, List())
												case None => info_in.head}
				case CFG.Expression(e,_) => expressionType(e,info_in.head, List())
				case CFG.Assignment(i,oe,_) => oe match {															
												case Some (e) => e match {
																	case AST.AssignmentExpression (_,_,_) => expressionType(e,info_in.head,List(i))         
																	case _ => var updatedMap = info_in.head+(i -> List(e))
																						updatedMap.foldLeft(updatedMap){case (map,(key,explist)) => map+(key -> explist.filter(exp => !isIdentifierInExpression(exp,i)))}}
												case None => info_in.head}
				case CFG.If(e,_) => expressionType(e,info_in.head, List())
				case CFG.DoWhile(e,_) => expressionType(e,info_in.head, List())
				case CFG.ForIn(e1,e2,_) => e1 match {
												case e1 : AST.Expression => expressionType(e2, expressionType (e1,info_in.head, List()), List())
												case _ => expressionType (e2, info_in.head, List())
				}
				case CFG.With(e,_) => expressionType(e,info_in.head, List())
				case CFG.Switch(e,_) => expressionType(e,info_in.head, List())
				case CFG.CaseClause(e,_) => expressionType(e,info_in.head, List())
				case _ => info_in.head
			}		
		  }
		  
		  def isIdentifierInExpression(exp : AST.Expression, iden : AST.Identifier) : Boolean = {
			exp match {
				case AST.ExpressionList(es) => es.foldLeft(false)((bool, exp) => bool || isIdentifierInExpression(exp,iden))
				case AST.AssignmentExpression(e1,op,e2) => isIdentifierInExpression(e1,iden) || isIdentifierInExpression(e2,iden)
				case AST.BinaryExpression(op,e1,e2) => isIdentifierInExpression(e1,iden) || isIdentifierInExpression(e2,iden)
				case AST.UnaryExpression(op,e) => isIdentifierInExpression(e,iden)
				case AST.PostfixExpression(op,e) => isIdentifierInExpression(e,iden)
				case AST.ConditionalExpression(e1,e2,e3) => isIdentifierInExpression(e1,iden) || isIdentifierInExpression(e2,iden) || isIdentifierInExpression(e3,iden)
				case AST.CallExpression(e,oes) => isIdentifierInExpression(e,iden) || (oes match {
																						case Some (es) => es.foldLeft(false)((bool, exp) => bool || isIdentifierInExpression(exp,iden))
																						case None => false})
				case AST.AllocationExpression(e) => isIdentifierInExpression(e,iden)
				case AST.ArrayAccessExpression(e1,e2) => isIdentifierInExpression(e1,iden) || isIdentifierInExpression(e2,iden)
				case AST.ObjectAccessExpression(e,i) => isIdentifierInExpression(e,iden)
				case AST.ArrayLiteral(oes) => oes match {
												case Some (es) => es.foldLeft(false)((bool, exp) => bool || isIdentifierInExpression(exp,iden))
												case None => false}
				case AST.Identifier(s) => exp==iden		// May have to compare on the string
				case _ => false
			}
		  } 
	}

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
			case AST.AssignmentExpression (e1,op,e2) => e2 match {
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

	 
	def graph(n : String, cfg : CFG.ControlFlowGraph, cseMap : Map[(CFG.ControlFlowNode, CFG.ControlFlowNode), Map[AST.Identifier, List[AST.Expression]]], csfeMap: Map[AST.Identifier, Map[(CFG.ControlFlowNode, CFG.ControlFlowNode), Map[AST.Identifier, List[AST.Expression]]]], startEndNodes : Boolean = true) : GraphvizDrawer.Graph = {
		new GraphvizDrawer.Graph {
			var start = GraphvizDrawer.Node("start", "Start", Some(GraphvizDrawer.Diamond()))
			var end = GraphvizDrawer.Node("end", "End", Some(GraphvizDrawer.Square()))

			def nodes() = {
				var startList = if (startEndNodes) List(start,end) else List()
				cfg.nodes.foldLeft(startList)((list,node) => {
					GraphvizDrawer.Node(nodeId( node ), GraphvizDrawer.escape( nodeToString( node ) ) ) :: list
				})
			}
			def edges() = {
				var startList = if (startEndNodes) {
					var edge1 = GraphvizDrawer.Edge("start",nodeId(cfg.start))
			 		var edge2 = GraphvizDrawer.Edge(nodeId(cfg.end),"end")
			 		List(edge1,edge2)
				} else {
					List()
				}
				cfg.edges.foldLeft(startList)((list,edge) => {
					var (from,to) = edge
					println("From edges: " + cseMap)
					GraphvizDrawer.Edge(nodeId(from),nodeId(to), Option (cseElemToString(cseMap.get(from,to)))) :: list
				})
			}
			
			def subgraphs() = {
				cfg.info.functions.foldLeft(List() : List[GraphvizDrawer.Graph])((list,pair) => {
					var (name,func) = pair
					println(func.name + ": " + csfeMap.getOrElse(func.name,Map()))
					graph(func.name + "("+AST.printList(func.params,", ")+")",func.cfg, csfeMap.getOrElse(func.name,Map()),csfeMap,false) :: list
				})
			}
			
			def name() = n
		}
	}
	
	def cseElemToString(elem : Option[Map[AST.Identifier, List[AST.Expression]]]) : String = {
		elem match {
			case Some (e) => e.foldLeft(""){ case (string,(key,value)) => if(value.size>0){
																			GraphvizDrawer.escape(string + key + " => " + value.head + "\n")
																		  }else{
																		     string}}																			
			case None => "" // not used
		}
	}
	
	
}