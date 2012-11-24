package JSAnalyzer

object GlobalFlowFunction{
	abstract class GFFunction[t] {
		def globalFlowFunction(node:CFG.ControlFlowNode, info_in : List[t], lattice : Lattice.AbstractLattice[t]) : t;
	}
	
	class CSEFlowFunction extends GlobalFlowFunction.GFFunction[Map[AST.Identifier, List[AST.Expression]]] {
	  def globalFlowFunction(node:CFG.ControlFlowNode, info_in : List[Map[AST.Identifier, List[AST.Expression]]], lattice : Lattice.AbstractLattice[Map[AST.Identifier, List[AST.Expression]]])= {
		node match {
	//		case CFG.Merge(_,_) => lattice.getGlb(info_in(0),info_in(1)) //TODO
			case CFG.Continue(oe,_) => Map() //TODO
			case CFG.Return(e,_) => Map() //TODO
			case CFG.Expression(e,_) => Map() //TODO
			case CFG.Assignment(i,oe,_) => oe match {
											case Some (e) => e match {
																case AST.AssignmentExpression (e1,op,e2) => Map() //TODO
																case _ => println("TESTT");info_in.head+(i -> List(e))} //TODO
											case None => info_in.head}
			case CFG.If(e,_) => Map() //TODO
			case CFG.DoWhile(e,_) => Map() //TODO
			case CFG.ForIn(e1,e2,_) => Map() //TODO
			case CFG.With(e,_) => Map() //TODO
			case CFG.Switch(e,_) => Map() //TODO
			case CFG.CaseClause(e,_) => Map() //TODO
			case _ => Map()
		}
	  }
	}
}

