package JSAnalyzer

object DataFlowAnalysis{
	abstract class DataFlowAnalysis[t] {
	
		def globalFlowFunction(node:CFG.ControlFlowNode, info_in : List[t], dataAnalyzer : DataFlowAnalysis[t]) : t
		
		def getBottom : t
	  
		def getTop : t
	   
		def compareElements(e1:t,e2:t) : Option[Boolean]
	  
		def getLub(e1:t, e2:t) : t
	  
		def getGlb(e1:t, e2:t) : t
	}
	
	def worklistalgorithm[t](dataAnalyzer : DataFlowAnalysis[t], cfg : CFG.ControlFlowGraph) : Map[(CFG.ControlFlowNode, CFG.ControlFlowNode), t] = {
		var worklistStart : List[CFG.ControlFlowNode] = cfg.nodes;
		var mapStart = cfg.edges.foldLeft(Map[(CFG.ControlFlowNode, CFG.ControlFlowNode),t]())((map,edge) => map+(edge -> dataAnalyzer.getBottom))

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

				var info_out = dataAnalyzer.globalFlowFunction(node, info_in, dataAnalyzer);
				iterateWorklist(
					outgoingEdges.foldLeft((newWorklist,map))((info,edge) => {
						var (newlist,newmap) = info
						(dataAnalyzer.compareElements(dataAnalyzer.getLub(newmap(edge),info_out), newmap(edge)),dataAnalyzer.compareElements(newmap(edge), dataAnalyzer.getLub(newmap(edge),info_out))) match {
							case (Some (true), Some (true)) => (newlist,newmap)
							case (_,_) => var newmap2 = newmap+(edge -> dataAnalyzer.getLub(newmap(edge),info_out))
										  var (from,to) = edge
										 (to::newlist,newmap2)
					}}))
		}
		}
			var (_,newMap) = iterateWorklist((worklistStart,mapStart))	
			newMap
		}	

}