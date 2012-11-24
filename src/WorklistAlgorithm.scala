package JSAnalyzer

object WorklistAlgorithm{
	def worklistalgorithm[t](globalflowfunction : GlobalFlowFunction.GFFunction[t], lattice : Lattice.AbstractLattice[t], cfg : CFG.ControlFlowGraph) : Map[(CFG.ControlFlowNode, CFG.ControlFlowNode), t] = {
	  var worklistStart : List[CFG.ControlFlowNode] = cfg.nodes;
	  var mapStart = cfg.edges.foldLeft(Map[(CFG.ControlFlowNode, CFG.ControlFlowNode),t]())((map,edge) => map+(edge -> lattice.getBottom))
	  	  
		  
	  def iterateWorklist(info : (List[CFG.ControlFlowNode], Map[(CFG.ControlFlowNode, CFG.ControlFlowNode), t])) : (List[CFG.ControlFlowNode], Map[(CFG.ControlFlowNode, CFG.ControlFlowNode), t]) = {
		  var (list,map) = info
		  list.size match {
		  	case 0 => (list,map)
		  	case _ => 
				var node : CFG.ControlFlowNode = list.head;
				var newWorklist : List[CFG.ControlFlowNode] = list.slice(1,list.size);
				var incomingEdges : List[(CFG.ControlFlowNode, CFG.ControlFlowNode)] = cfg.edges.filter{case(from,to) => to==node};
				var outgoingEdges : List[(CFG.ControlFlowNode, CFG.ControlFlowNode)] = cfg.edges.filter{case(from,to) => from==node};
				var info_in : List[t] = incomingEdges.foldLeft(List[t]())((list,edge) => map.get(edge) match {
				  case Some(e) => e :: list
				  case None => list
				})
				println("TEST1")
				var info_out = globalflowfunction.globalFlowFunction(node, info_in, lattice);
				println("TEST2")
				iterateWorklist(
				    outgoingEdges.foldLeft((newWorklist,map))((info,edge) => {
						println("TEST3")
						var (newlist,newmap) = info
						println("TEST4")
						(lattice.compareElements(lattice.getLub(newmap(edge),info_out), newmap(edge)),lattice.compareElements(newmap(edge), lattice.getLub(newmap(edge),info_out))) match {
							case (Some (true), Some (true)) => println("TEST5");(newlist,newmap)
							case (_,_) => println("TEST6"); var newmap2 = newmap+(edge -> lattice.getLub(newmap(edge),info_out))
										  var (from,to) = edge
										 (to::newlist,newmap2)
					}}))
	  }
	}
	println("TEST")
	var (_,newMap) = iterateWorklist((worklistStart,mapStart))	
	newMap
	}	
}




