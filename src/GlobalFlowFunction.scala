package JSAnalyzer

object GlobalFlowFunction{
	abstract class GFFunction[t] {
		def globalFlowFunction(node:CFG.ControlFlowNode, info_in : List[t]) : t;
	}
	
	class TestFlowFunction extends GFFunction[Int]{
	  def globalFlowFunction(node:CFG.ControlFlowNode, info_in : List[Int]) : Int = {
	    node match {
	      case node : CFG.If => 4
	      case node : CFG.Empty => 4
	      case node : CFG.Expression => 4
	      case node : CFG.Merge => 4/info_in.size
	      case _ => info_in(0)
	    }
	  }
	}
}

