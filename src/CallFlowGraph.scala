package JSAnalyzer

import java.util.UUID

/** TODO: 
	* Remove Empty nodes
	* functionDeclaration
	* CaseBlocks should be walked though and if a break is caught this should be updated
	* Continue and breaks needs to be dealt with
**/

object CallFG {
	case class Node
	case class MainNode(id: String = UUID.randomUUID().toString()) extends Node
	case class FunctionNode(func: AST.Identifier, params: List[AST.Identifer], id: String = UUID.randomUUID().toString()) extends Node

	abstract class Edge
	case class CallEdge(from:Node ,to: Node, CallNode: CFG.ControlFlowNode) extends Edge
	case class ReturnEdge(to:Node ,from: Node, callEdge: CallEdge, returnNode: CFG.ControlFlowNode) extends Edge

	case class CallFlowGraph(nodes: List[Node], edges: List[Edge], main: Option[MainNode])

	def combine(c1: CallFlowGraph, c2: CallFlowGraph, main: Option[MainNode]) : CallFlowGraph = {
		CallFlowGraph(c1.nodes :: c2.nodes,c1.edges :: c2.edges, main)
	}

	def createCFG(cfg: CFG.ControlFlowGraph) : CallFlowGraph = {
		var nodes = cfg.info.functions.foldLeft(List(MainNode()))((list,func) => FunctionNode(func.name, func.params) :: list)

		CallFlowGraph(nodes, List())
	}

	def graph(c: CallFlowGraph) : GraphvizDrawer.Graph = {
		new GraphvizDrawer.Graph {
			var start = GraphvizDrawer.Node("start", "Start", Some(GraphvizDrawer.Diamond()))
			var end = GraphvizDrawer.Node("end", "End", Some(GraphvizDrawer.Square()))

			def nodeToString(node) => node match {
				case MainNode(id) => "MAIN"
				case FunctionNode(func,params,id) => func.value +"("+AST.printList(params,",")+")"
			}

			def nodeId(node) => node match {
				case MainNode(id) => id
				case FunctionNode(_,_,id) => id
			}

			def nodes() = {
				var startList = c.main match {
					case Some(n) => List(start,end)
					case None() => List()
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
				cfg.edges.foldLeft(startList)((list,edge) => {
					edge match {
						case 
					}
					var (from,to) = edge
					GraphvizDrawer.Edge(nodeId(from),nodeId(to), ) :: list
				})
			}

			def subgraphs() = {
				cfg.info.functions.foldLeft(List() : List[GraphvizDrawer.Graph])((list,pair) => {
					var (name,func) = pair
					graph(func.name + "("+AST.printList(func.params,", ")+")",func.cfg,false) :: list
				})
			}
			
			def name() = n
		}
	}

}