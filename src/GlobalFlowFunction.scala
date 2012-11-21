package JSAnalyzer

object GlobalFlowFunction{
	abstract class GFFunction[t] {
		def globalFlowFunction(node:CFG.ControlFlowNode, info_in : List[t]) : t;
	}
}

