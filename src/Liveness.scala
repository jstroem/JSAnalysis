package JSAnalysis


object Liveness {
	
	class LivenessLatice(variables: List[AST.Identifier]) extends Lattice.AbstractLattice[List[AST.Identifier]], GlobalFlowFunction.GFFunction[List[AST.Identifier]] {
			  
	  def getBottom : List[AST.Identifier] = List()
	  
	  def getTop : List[AST.Identifier] = variables
	   
	  def compareElements(m1:List[AST.Identifier],m2:List[AST.Identifier]) : Option[Boolean] = {
	  	var shared = m1.intersect(m2)
	  	if (shared.size == 0) None()
	  	else if (shared == m1) Some(true)
	  	else Some(false)
	  }
	  
	  def getLub(m1:Map[AST.Identifier, List[AST.Expression]], m2:Map[AST.Identifier, List[AST.Expression]]) : List[AST.Identifier] = {
	  	m1.union(m2)
	  }
	  
	  def getGlb(m1: List[AST.Identifier], m2:List[AST.Identifier]) : List[AST.Identifier] = {
		m1.intersect(m2)
	  }

	  def globalFlowFunction(node:CFG.ControlFlowNode, info_out : List[AST.Identifier], lattice : Lattice.AbstractLattice[List[AST.Identifier]])= node match {
	//		case CFG.Merge(_,_) => lattice.getGlb(info_in(0),info_in(1)) //TODO
			case CFG.Continue(oi,_) => info_out
			case CFG.Return(e,_) => expression(e,info_out)
			case CFG.Expression(e,_) => expression(e,info_out)
			case CFG.Assignment(i,oe,_) => oe match {
											case Some (e) => {
												if info_out.contains(i) {
													expression(e,info_out.filter((node) => node != i))
												} else {
													info_out
												}
											case None => info_out}
			case CFG.If(e,_) => expression(e,info_out)
			case CFG.DoWhile(e,_) => expression(e,info_out)
			case CFG.ForIn(e1,e2,_) => expression(e2,info_out) ::: expression(e1,info_out)
			case CFG.With(e,_) => expression(e,info_out)
			case CFG.Switch(e,_) => expression(e,info_out)
			case CFG.CaseClause(e,_) => expression(e,info_out)
			case DefaultClause(_) => info_out
			case CFG.Empty(_) => info_out
			case CFG.Merge(_,_) => info_out
		}
	  }
	}

	def expression(e : AST.Expression, info_out: List[AST.Identifier]) : List[AST.Identifier] = e match {
		case AST.ExpressionList(lst) => lst.foldLeft(info_out)((list,exp) => expression(exp,list))
		case AST.AssignmentExpression(lhs, op, rhs) => lhs match {
			case lhs: AST.Identifier => {
				if info_out.contains(lhs) {
					expression(e,info_out.filter((node) => node != lhs))
				} else {
					info_out
				}
			}
			case _ => expression(rhs) //Cant figure anything out so every item used in the rhs is in use
		}
		case AST.FunctionExpression(name, params, body) => info_out
		case AST.BinaryExpression(op, t1, t2) => expression(t2,expression(t1,info_out))
		case AST.UnaryExpression(op, t1) => expression(t1,info_out)
		case AST.PostfixExpression(op, t1) => info_out // x = x + 1 does remove x and adds x. so nothing needs to be done
		case AST.ConditionalExpression(cond, ifBranch, elseBranch) => expression(elseBranch,expression(ifBranch,expression(cond,info_out)))
		case AST.CallExpression(callable, args) => args.getOrElse(List()).foldLeft(expression(callable,info_out))((list,exp) => expression(exp,list))
		case AST.AllocationExpression(exp) => expression(exp,info_out)
		case MemberAccessExpression() => info_out
		case ObjectAccessExpression(obj, member) => expression(obj,info_out)
		case Undefined() => info_out
		case Literal() => info_out
		case Identifier(v) => e :: info_out
	}

	def variables(cfg : CFG.ControlFlowNode) : List[AST.Identifier] = {
		cfg.nodes.foldLeft(List())((list,node) => node match {
			case CFG.Assignment(i,_,_) => i :: list
			case _ => list
		})
	}

}