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
				var info_out = globalflowfunction.globalFlowFunction(node, info_in);
				iterateWorklist(
				    outgoingEdges.foldLeft((newWorklist,map))((info,edge) => {
						var (newlist,newmap) = info
						(lattice.compareElements(lattice.getLub(newmap(edge),info_out), newmap(edge)),lattice.compareElements(newmap(edge), lattice.getLub(newmap(edge),info_out))) match {
							case (Some (true), Some (true)) => (newlist,newmap)
							case (_,_) => var newmap2 = newmap+(edge -> lattice.getLub(newmap(edge),info_out))
										  var (from,to) = edge
										 (to::newlist,newmap2)
					}}))
	  }
	}
	var (_,newMap) = iterateWorklist((worklistStart,mapStart))	
	newMap
	}	
}




