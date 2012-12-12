package JSAnalyzer

import java.util.UUID

object CallFG {
	abstract class Node
	case class MainNode(id: String = UUID.randomUUID().toString()) extends Node
	case class FunctionNode(func: AST.Identifier, params: List[AST.Identifier], id: String = UUID.randomUUID().toString()) extends Node

	abstract class Edge
	case class CallEdge(from:Node ,to: Node, callNode: CFG.ControlFlowNode) extends Edge
	case class ReturnEdge(from: Node, to:Node, callEdge: CallEdge, returnNode: CFG.ControlFlowNode) extends Edge

	case class CallFlowGraph(nodes: List[Node], edges: List[Edge], main: Option[MainNode])

	def combine(c1: CallFlowGraph, c2: CallFlowGraph, main: Option[MainNode]) : CallFlowGraph = {
		CallFlowGraph(c1.nodes ++ c2.nodes,c1.edges ++ c2.edges, main)
	}

	def createCFG(cfg: CFG.ControlFlowGraph) : CallFlowGraph = {
		var main = MainNode()
		var nodes = cfg.info.functions.foldLeft(List(main) : List[Node])((list,pair) => {
			var (name,func) = pair
			FunctionNode(func.name, func.params) :: list
		})
		var map = nodes.foldLeft(Map() : Map[AST.Identifier, Node])((map,node) => node match {
			case FunctionNode(name, params,_) => map + ((name,node))
			case MainNode(_) => map
		})
		var edges = cfg.info.functions.foldLeft(walkCfgNodes(cfg, main, cfg.info, map))((list,pair) => {
			var (name,func) = pair
			map.get(func.name) match {
				case Some(node) => walkCfgNodes(func.cfg, node, cfg.info, map) ++ list
				case None => list
			}
		})
		CallFlowGraph(nodes, edges, Some(main))
	}

	def walkCfgNodes(cfg : CFG.ControlFlowGraph, fromNode : Node, info: CFG.Info, map : Map[AST.Identifier, Node]) : List[Edge] = {
		cfg.nodes.foldLeft(List() : List[Edge])((list,node) => {
			node match {
				case CFG.Expression(e,_) => walkExpression(e, fromNode, node, info, map) ++ list
				case CFG.Return(oe,_) => oe match {
					case Some(e) => walkExpression(e, fromNode, node, info, map) ++ list
					case None => list
				}
				case CFG.Assignment(_,oe,_) => oe match {
					case Some(e) => walkExpression(e, fromNode, node, info, map) ++ list
					case None => list
				}
				case CFG.If(e, _) => walkExpression(e, fromNode, node, info, map) ++ list
				case CFG.DoWhile(e, _) => walkExpression(e, fromNode, node, info, map) ++ list
				case CFG.ForIn(_, e, _) => walkExpression(e, fromNode, node, info, map) ++ list
				case CFG.With(e,_) => walkExpression(e, fromNode, node, info, map) ++ list
				case CFG.Switch(e,_) => walkExpression(e, fromNode, node, info, map) ++ list
				case CFG.CaseClause(e,_) => walkExpression(e, fromNode, node, info, map) ++ list
				case _ => list
			}
		})
	}

	def walkExpression( e : AST.Expression, fromNode : Node, cfgNode : CFG.ControlFlowNode, info : CFG.Info, map: Map[AST.Identifier, Node]) : List[Edge] = {
		e match {
			case AST.ExpressionList(lst) => lst.foldLeft(List() : List[Edge])((list,exp) => walkExpression(exp,fromNode,cfgNode,info,map) ++ list)
			case AST.AssignmentExpression(lhs,op,rhs) => walkExpression(lhs,fromNode,cfgNode,info,map) ++ walkExpression(rhs,fromNode,cfgNode,info,map)
			case AST.BinaryExpression(op,t1,t2) => walkExpression(t1,fromNode,cfgNode,info,map) ++ walkExpression(t2,fromNode,cfgNode,info,map)
			case AST.UnaryExpression(op,t1) => walkExpression(t1,fromNode,cfgNode,info,map)
			case AST.PostfixExpression(op,t1) => walkExpression(t1,fromNode,cfgNode,info,map)
			case AST.ConditionalExpression(cond,ifCase,elseCase) => walkExpression(cond,fromNode,cfgNode,info,map) ++ walkExpression(ifCase,fromNode,cfgNode,info,map) ++ walkExpression(elseCase,fromNode,cfgNode,info,map)
			case AST.CallExpression(called : AST.Identifier,args) => {
				info.functions.foldLeft(List() : List[Edge])((list,pair) => {
					var (callToname,callTofunc) = pair
					if (called == callToname) {
						map.get(callTofunc.name) match {
							case Some(callToNode) => makeEdges(fromNode, callToNode, cfgNode, callTofunc)
							case None => list
						}
					} else {
						list
					}
				})
			}
			case AST.AllocationExpression(exp) => walkExpression(exp,fromNode,cfgNode,info,map)
			case AST.ArrayAccessExpression(array, member) => walkExpression(array,fromNode,cfgNode,info,map) ++ walkExpression(member,fromNode,cfgNode,info,map)
			case AST.ObjectAccessExpression(array, member) => walkExpression(array,fromNode,cfgNode,info,map) ++ walkExpression(member,fromNode,cfgNode,info,map)
			case _ => List()
		}
	} 

	def makeEdges(fromNode : Node, toNode : Node, node : CFG.ControlFlowNode, func : CFG.Function) : List[Edge] = {
		var call = CallEdge(fromNode,toNode,node)
		func.returns.foldLeft(List(call) : List[Edge])((list,rtrn) => ReturnEdge(toNode, fromNode, call, rtrn) :: list)
	}

	def graph(n: String, c: CallFlowGraph) : GraphvizDrawer.Graph = {
		new GraphvizDrawer.Graph {
			var start = GraphvizDrawer.Node("start", "Start", Some(GraphvizDrawer.Diamond()))
			var end = GraphvizDrawer.Node("end", "End", Some(GraphvizDrawer.Square()))

			def nodeToString(node : Node) = node match {
				case MainNode(id) => "MAIN"
				case FunctionNode(func,params,id) => func.value +"("+AST.printList(params,",")+")"
			}

			def nodeId(node : Node) = node match {
				case MainNode(id) => id
				case FunctionNode(_,_,id) => id
			}

			def nodes() = {
				var startList = c.main match {
					case Some(n) => List(start,end)
					case None => List()
				}
				c.nodes.foldLeft(startList)((list,node) => {
					GraphvizDrawer.Node(nodeId( node ), GraphvizDrawer.escape( nodeToString( node ) ) ) :: list
				})
			}

			def edges() = {
				var startList = c.main match {
					case Some(n) => {
						var edge1 = GraphvizDrawer.Edge("start",nodeId(n))
				 		var edge2 = GraphvizDrawer.Edge(nodeId(n),"end")
				 		List(edge1,edge2)
					}
					case None => List()
				}
				c.edges.foldLeft(startList)((list,edge) => edge match {
					case CallEdge(from, to, callNode) => GraphvizDrawer.Edge(nodeId(from),nodeId(to)) :: list
					case ReturnEdge(from, to, callEdge, returnNode) => GraphvizDrawer.Edge(nodeId(from),nodeId(to))  :: list
				})
			}

			def subgraphs() = List()
			
			def name() = n
		}
	}

}