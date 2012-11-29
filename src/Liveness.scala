package JSAnalyzer


object Liveness {
	
	class LivenessAnalysis(variables: List[AST.Identifier]) extends DataFlowAnalysis.DataFlowAnalysis[List[AST.Identifier]] {
			  
	  def getBottom : List[AST.Identifier] = List()
	  
	  def getTop : List[AST.Identifier] = variables
	   
	  def compareElements(m1:List[AST.Identifier],m2:List[AST.Identifier]) : Option[Boolean] = {
	  	var shared = m1.intersect(m2)
	  	if (shared.size == 0) None
	  	else if (shared == m1) Some(true)
	  	else Some(false)
	  }
	  
	  def getLub(m1:List[AST.Identifier], m2:List[AST.Identifier]) : List[AST.Identifier] = {
	  	m1.union(m2).distinct
	  }
	  
	  def getGlb(m1: List[AST.Identifier], m2:List[AST.Identifier]) : List[AST.Identifier] = {
		m1.intersect(m2).distinct
	  }

	  def globalFlowFunction(node:CFG.ControlFlowNode, info_in : List[List[AST.Identifier]], dataAnalyzer : DataFlowAnalysis.DataFlowAnalysis[List[AST.Identifier]]) = {
		var info = info_in.foldLeft(dataAnalyzer.getBottom)((info1,info2) => dataAnalyzer.getLub(info1,info2))
		node match {
			case CFG.Continue(oi,_) => info
			case CFG.Return(e,_) => e match {
				case Some(e) => expression(e,info)
				case None => info
			}
			case CFG.Expression(e,_) => expression(e,info)
			case CFG.Assignment(i,oe,_) => oe match {
											case Some (e) => {
												if (info.contains(i)) {
													expression(e,info.filter((node) => node != i))
												} else {
													info
												}
											}
											case None => info
			}
			case CFG.If(e,_) => expression(e,info)
			case CFG.DoWhile(e,_) => expression(e,info)
			case CFG.ForIn(e1,e2,_) => e1 match { // e1 is an variable here and is only defined in the for statement so it should just be removed from the info.
				case e1 : AST.Identifier => expression(e2,info.filter((node) => node != e1))
				case _ => expression(e2,info) 
			}
			case CFG.With(e,_) => expression(e,info)
			case CFG.Switch(e,_) => expression(e,info)
			case CFG.CaseClause(e,_) => expression(e,info)
			case CFG.DefaultClause(_) => info
			case CFG.Empty(_) => info
			case CFG.Break(_) => info
			case CFG.Merge(_,_) => info
		}
	  }

	def expression(e : AST.Expression, info_in: List[AST.Identifier]) : List[AST.Identifier] = e match {
		case AST.ExpressionList(lst) => lst.foldLeft(info_in)((list,exp) => expression(exp,list))
		case AST.AssignmentExpression(lhs, op, rhs) => lhs match {
			case lhs: AST.Identifier => {
				if (info_in.contains(lhs)) {
					expression(rhs,info_in.filter((node) => node != lhs))
				} else {
					info_in
				}
			}
			case _ => expression(rhs,info_in) //Cant figure anything out so every item used in the rhs is in use
		}
		case AST.FunctionExpression(name, params, body) => info_in
		case AST.BinaryExpression(op, t1, t2) => expression(t2,expression(t1,info_in))
		case AST.UnaryExpression(op, t1) => expression(t1,info_in)
		case AST.PostfixExpression(op, t1) => info_in // x = x + 1 does remove x and adds x. so nothing needs to be done
		case AST.ConditionalExpression(cond, ifBranch, elseBranch) => expression(elseBranch,expression(ifBranch,expression(cond,info_in)))
		case AST.CallExpression(callable, args) => args.getOrElse(List()).foldLeft(expression(callable,info_in))((list,exp) => expression(exp,list))
		case AST.AllocationExpression(exp) => expression(exp,info_in)
		case AST.ArrayAccessExpression(e1,e2) => expression(e2,expression(e1,info_in))
		case AST.ObjectAccessExpression(obj, member) => expression(obj,info_in)
		case AST.Undefined() => info_in
		case e : AST.Identifier => if (info_in.contains(e)) info_in else e :: info_in
		case e : AST.Literal => info_in
	}
  }

	def variables(cfg : CFG.ControlFlowGraph) : List[AST.Identifier] = {
		cfg.nodes.foldLeft(List() : List[AST.Identifier])((list,node) => node match {
			case CFG.Assignment(i,_,_) => i :: list
			case _ => list
		})
	}

	def graph(n: String, cfg : CFG.ControlFlowGraph, liveness: Map[(CFG.ControlFlowNode, CFG.ControlFlowNode),List[AST.Identifier]], fliveness: Map[AST.Identifier, Map[(CFG.ControlFlowNode, CFG.ControlFlowNode),List[AST.Identifier]]],startEndNodes : Boolean = true) : GraphvizDrawer.Graph = {
		new GraphvizDrawer.Graph {
			var start = GraphvizDrawer.Node("start", "Start", Some(GraphvizDrawer.Diamond()))
			var end = GraphvizDrawer.Node("end", "End", Some(GraphvizDrawer.Square()))

			def nodes() = {
				var startList = if (startEndNodes) List(start,end) else List()
				cfg.nodes.foldLeft(startList)((list,node) => {
					GraphvizDrawer.Node(CFGGrapher.nodeId( node ), GraphvizDrawer.escape( CFGGrapher.nodeToString( node ) ) ) :: list
				})
			}
			def edges() = {
				var startList = if (startEndNodes) {
					var edge1 = GraphvizDrawer.Edge("start",CFGGrapher.nodeId(cfg.start))
			 		var edge2 = GraphvizDrawer.Edge(CFGGrapher.nodeId(cfg.end),"end")
			 		List(edge1,edge2)
				} else {
					List()
				}
				cfg.edges.foldLeft(startList)((list,edge) => {
					var (from,to) = edge
					GraphvizDrawer.Edge(CFGGrapher.nodeId(from),CFGGrapher.nodeId(to), Some(GraphvizDrawer.escape("{"+AST.printList(liveness.getOrElse((to,from),List()),",")+"}"))) :: list
				})
			}

			def subgraphs() = {
				cfg.info.functions.foldLeft(List() : List[GraphvizDrawer.Graph])((list,pair) => {
					var (name,func) = pair
					graph(func.name + "("+AST.printList(func.params,", ")+")",func.cfg, fliveness.getOrElse(func.name,Map()), fliveness,false) :: list
				})
			}
			
			def name() = n
		}
	}
}