package JSAnalyzer

import scala.collection.mutable.HashMap

object Dominance {
  type infoT = Map[CFG.ControlFlowNode, Set[CFG.ControlFlowNode]]
  
  class DominanceConstructor(val cfg : CFG.ControlFlowGraph) {
    val bottom = Set(cfg.nodes: _*)
    val top = Set(cfg.nodes.head)
    
    def flowFunction(info_in1 : List[Set[CFG.ControlFlowNode]], node : CFG.ControlFlowNode) : Set[CFG.ControlFlowNode] = {
      val info_in = if (info_in1.length == 0) top :: info_in1 else info_in1
      val info_out = node match {
        case node : CFG.Merge => info_in.foldLeft(bottom) { (map, e) => map.intersect(e) }
        case _ => info_in.head
      }
      info_out.union(Set(node))
    }
    
    def printWorklist(worklist : List[CFG.ControlFlowNode]) = {
      "[ " + worklist.map(CFGGrapher.nodeToString(_)).mkString(", ") + " ]"
    }
    
    def dom() : infoT = {
      val worklistStart: List[CFG.ControlFlowNode] = cfg.nodes
      val mapStart = cfg.nodes.map { n : CFG.ControlFlowNode => if (n != cfg.nodes.head) (n, bottom) else (n, Set(n)) } toMap
      
      def iterateWorklist(list : List[CFG.ControlFlowNode], map : infoT) : infoT = {
        list.size match {
          case 0 => map
          case _ =>
            val node = list.head
            val incomingEdges = cfg.edges.filter { case (from, to) => to == node }
            val outgoingEdges = cfg.edges.filter { case (from, to) => from == node }
            val info_in = incomingEdges.map { case (from, to) => map(from) }

            val info_out = flowFunction(info_in, node)
            
            val newWorklist = (list.slice(1, list.size) ::: (if (map(node).diff(info_out).isEmpty) List() else outgoingEdges.map { case (from, to) => to })).distinct
            
            iterateWorklist(newWorklist, map.map { case (key, value) => (key, if (key == node) info_out else value) } toMap)
        }
      }
      iterateWorklist(worklistStart, mapStart)
    }

    def idom(dom : infoT) = {
      // The immediate dominator or idom of a node n is the unique node that 
      // strictly dominates n but does not strictly dominate any other node 
      // that strictly dominates n.

      dom.-(cfg.nodes.head).map {
        case (node, doms) => (node, (doms - node).map {
          e =>
            if (doms.-(node).foldLeft(true) { (bool, cand) => bool && dom(e).contains(cand) }) {
              List(e)
            } else {
              List()
            }
        } flatten)
      }.toMap + (cfg.nodes.head -> Set(cfg.nodes.head))
    }
    
    def domFront(idom : infoT) = {
      val res = HashMap[CFG.ControlFlowNode, Set[CFG.ControlFlowNode]]()

      cfg.nodes.foreach(node => {
        val preds = cfg.edges.filter{case (from, to) => to == node}.map{ case (from, to) => from }
        preds.foreach(p => {
          var runner = p
          while (runner != idom(runner).head && runner != idom(node).head) {
            val oldSet = res.get(runner)
            res.put(runner, oldSet.getOrElse(Set()) + node)
            runner = idom(runner).head
          }
        })
      })
      
      res toMap
    }
    
    def printDF(df : infoT) = {
      df.foreach{case (key, value) => {
        println("DF(" + CFGGrapher.nodeToString(key) + ") = " + printWorklist(value toList))
      }}
    }
    
    def printIDom(idom : infoT) = {
      idom.foreach{case (key, value) => {
        println("Idom(" + CFGGrapher.nodeToString(key) + ") = " + printWorklist(value toList))
      }}
    }
  }
  
  def makeGraph(cfg : CFG.ControlFlowGraph, dominance : infoT) : CFG.ControlFlowGraph = {
    val edges = dominance.map { case (node, doms) => doms.map { (_, node) } }.toList.flatten.distinct
    CFG.ControlFlowGraph(cfg.start, cfg.end, cfg.nodes, edges, edges.map { case x => (x -> "") } toMap, cfg.info)
  }
}
