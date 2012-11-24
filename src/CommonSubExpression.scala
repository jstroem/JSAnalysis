package JSAnalyzer

object CSE {
	def extractIdentifiers(cfg : CFG.ControlFlowGraph) : List[AST.Identifier] = {
		cfg.nodes.foldLeft(List[AST.Identifier]())((list,node) => node match {
			case CFG.Assignment (i,e,_) => i::list; 
			case _ => list;
		})
	}
	
	def extractExpressionAssignments(cfg : CFG.ControlFlowGraph) : List[AST.Expression] = {
		def expressionType(exp : AST.Expression, expList : List[AST.Expression]) : List[AST.Expression] = {
			exp match {
			case AST.ExpressionList (es) => es.foldLeft(expList)((list,exp) => 
																			expressionType(exp,list))
			case AST.AssignmentExpression (e1,op,e2) => println("ASSIGNEXP" + e1 + op + "(" + e2 + ")"); 
														e2 match {
															case AST.AssignmentExpression (_,_,_) => 																
																expressionType(e2, expList)
															case _ => 
																expressionType(e2, e2::expList)
														}
			case AST.BinaryExpression (op,e1,e2) => expressionType(e2, expressionType (e1,expList))
			case AST.UnaryExpression (op, e) => expressionType (e,expList)
			case AST.PostfixExpression (op,e) => expressionType (e,expList)
			case AST.ConditionalExpression (e1,e2,e3) => expressionType(e3,expressionType(e2, expressionType (e1,expList)))
			case AST.CallExpression (e,es) => expressionType(e, es match {
																	case Some (es) => es.foldLeft(expList)((list,exp) => 
																					expressionType(exp,list))
																	case None => expList})	
			case AST.AllocationExpression (e) => expressionType (e,expList)
			case AST.ArrayAccessExpression (e1,e2) => expressionType(e2, expressionType (e1,expList))
			case AST.ObjectAccessExpression (e,i) => expressionType (e,expList)
			case AST.ArrayLiteral (es) =>  es match {
											case Some (es) => es.foldLeft(expList)((list,exp) => 
																					expressionType(exp,list))
											case None => expList}
			case _ => expList	
			}
		}
		
		cfg.nodes.foldLeft(List[AST.Expression]())((list,node) => node match {
			case CFG.Return (e,_) => e match {
										case Some (e) => expressionType(e,list)
										case None => list}
			case CFG.Expression (e,_) => expressionType(e, list)
			case CFG.Assignment (i,e,_) => e match {
												case Some (e) => e match {
																	case AST.AssignmentExpression (_,_,_) => expressionType(e,list)
																	case _ => e::list}
												case None => list}
			case CFG.If (e,_) => expressionType(e,list)
			case CFG.DoWhile (e,_)=> expressionType(e,list)
			case CFG.ForIn(e1,e2,_) => e1 match {
					case e1 : AST.Expression => expressionType(e2, expressionType (e1,list))
					case _ => expressionType (e2,list)
			}
			case CFG.With (e,_) => expressionType(e,list)
			case CFG.Switch (e,_) => expressionType(e,list)
			case CFG.CaseClause (e,_) => expressionType(e,list)
			case _ => list;
		})
	}	
	
	def getCSELatticeBottom(cfg : CFG.ControlFlowGraph) : Map[AST.Identifier, List[AST.Expression]] = {
		var idens : List[AST.Identifier] = extractIdentifiers(cfg)
		var exps : List[AST.Expression] = extractExpressionAssignments(cfg).distinct
		idens.foldLeft(Map() : Map[AST.Identifier, List[AST.Expression]])((map,identifier) => map+(identifier -> exps))
	}
	
	def getCSELatticeTop() : Map[AST.Identifier, List[AST.Expression]] = {
		Map()
	}
}