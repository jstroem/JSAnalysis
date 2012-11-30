package JSAnalyzer

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
    
    def dom() : infoT = {
      val worklistStart: List[CFG.ControlFlowNode] = cfg.nodes
      val mapStart = cfg.nodes.map { n : CFG.ControlFlowNode => if (n != cfg.nodes.head) (n, bottom) else (n, Set(n)) } toMap

      def printWorklist(worklist : List[CFG.ControlFlowNode]) = {
        "[ " + worklist.map(CFGGrapher.nodeToString(_)).mkString(", ") + " ]"
      }
      
      def iterateWorklist(list: List[CFG.ControlFlowNode], map : infoT) : infoT = {
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
  }
  
  def idom(dom : infoT) : infoT = {
    
  }
  
  def makeGraph(cfg : CFG.ControlFlowGraph, dominance : infoT) : CFG.ControlFlowGraph = {
    val edges = dominance.map { case (node, doms) => doms.map { (_, node) } }.toList.flatten.distinct
    CFG.ControlFlowGraph(cfg.start, cfg.end, cfg.nodes, edges, edges.map { case x => (x -> "") } toMap, cfg.info)
  }
}

/*
object Dominance {
  type Domain = List[CFG.ControlFlowNode]
  type Edge = (CFG.ControlFlowNode, CFG.ControlFlowNode)
  
  class DominanceConstructor(bottom : Domain, top: Domain) 
  extends DataFlowAnalysis.DataFlowAnalysis[Domain] {
    def getBottom : Domain = {
      bottom
	}

	def getTop : Domain = {
	  top
	}
	
	def getLub(m1 : Domain, m2 : Domain) : Domain = {
	  m1.union(m2).distinct
	}

    def getGlb(m1 : Domain, m2 : Domain) : Domain = {
	  m1.intersect(m2).distinct												
	}
    
    def compareElements(m1 : Domain, m2 : Domain) : Option[Boolean] = {
      if (m1.intersect(m2) == m1) {
        Some(true)
      } else if (m2.intersect(m1) == m2) {
        Some(false)
      } else {
        None
      }
    }
    
    def infoToString(info: Domain) = {
      var res = "["
      info.foreach { 
        res += CFGGrapher.nodeToString(_) + ", "
      }
      res + "]"
    }
    
    def globalFlowFunction(node : CFG.ControlFlowNode, info_in : List[Domain], lattice : DataFlowAnalysis.DataFlowAnalysis[Domain]) : Domain = {
      var info_in1 = if(info_in.isEmpty) lattice.getBottom::info_in else info_in
      
      println("Node: " + CFGGrapher.nodeToString(node))
      println("Info in: " + infoToString(info_in1.head))
      
      var info_out = node match {
        case node : CFG.Merge => info_in1.foldLeft(lattice.getTop)((map, e) => lattice.getGlb(map, e).distinct)
        case _ => info_in1.head
      }
      info_out = info_out.union(List(node)).distinct
      
      println("Info out: " + infoToString(info_out))
      info_out
    }
  }
  
  def getLatticeBottom(cfg : CFG.ControlFlowGraph) : Domain = {
    List(cfg.nodes(0))
  }
  
  def getLatticeTop(cfg : CFG.ControlFlowGraph) : Domain = {
    cfg.nodes
  }
  
  def makeGraph(cfg : CFG.ControlFlowGraph, dominance : Map[Edge, Domain]) : CFG.ControlFlowGraph = {
    val edges = dominance.map { case ((from, to), doms) => doms.map { (_, from) } }.toList.flatten.distinct
    CFG.ControlFlowGraph(cfg.start, cfg.end, cfg.nodes, edges, Map(edges.map { case x => (x -> "") }: _*), cfg.info)
  }
}*/

