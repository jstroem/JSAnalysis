package JSAnalyzer

object CSEFlowAnalysis{

	class CSELattice(bottom : Map[AST.Identifier, List[AST.Expression]], top: Map[AST.Identifier, List[AST.Expression]]) extends DataFlowAnalysis.DataFlowAnalysis[Map[AST.Identifier, List[AST.Expression]]] {
			  
	def getBottom : Map[AST.Identifier, List[AST.Expression]] = {
		bottom;
	}

		def getTop : Map[AST.Identifier, List[AST.Expression]] = {
		top;
		}

		def compareElements(m1:Map[AST.Identifier, List[AST.Expression]],m2:Map[AST.Identifier, List[AST.Expression]]) : Option[Boolean] = {
		if(m2.foldLeft(true){case (bool,(key,value)) => var m1values = m1.apply(key);
														bool && 
														value.foldLeft(true)((bool,elem) => m1values.contains(elem) && bool)}){
			Some (true)
		}else{			
			if(m1.foldLeft(true){case (bool,(key,value)) => var m2values = m2.apply(key);
															bool && 
															value.foldLeft(true)((bool,elem) => m2values.contains(elem) && bool)}){
				Some (false)
			}else{
				None
			}
		}
		}

		def getLub(m1:Map[AST.Identifier, List[AST.Expression]], m2:Map[AST.Identifier, List[AST.Expression]]) : Map[AST.Identifier, List[AST.Expression]] = {
			m1.foldLeft(Map():Map[AST.Identifier, List[AST.Expression]]){case (map,(key,value)) => map+(key -> value.intersect(m2.apply(key)))}
		}


		def getGlb(m1:Map[AST.Identifier, List[AST.Expression]], m2:Map[AST.Identifier, List[AST.Expression]]) : Map[AST.Identifier, List[AST.Expression]] = {
			m1.foldLeft(Map():Map[AST.Identifier, List[AST.Expression]]){case (map,(key,value)) => map+(key -> value.union(m2.apply(key)))}																	
		}
	  
	}

	class CSEFlowFunction extends DataFlowAnalysis.DataFlowAnalysis[Map[AST.Identifier, List[AST.Expression]]] {
	  def globalFlowFunction(node:CFG.ControlFlowNode, info_in1 : List[Map[AST.Identifier, List[AST.Expression]]], lattice : DataFlowAnalysis.DataFlowAnalysis[Map[AST.Identifier, List[AST.Expression]]])= {
		
		var info_in = if(info_in1.isEmpty){lattice.getTop::info_in1}else{info_in1}
		
		def expressionType(exp : AST.Expression, infoMap : Map[AST.Identifier, List[AST.Expression]], identifiers : List[AST.Identifier]) : Map[AST.Identifier, List[AST.Expression]] = {
			exp match {
				case AST.ExpressionList (es) => es.foldLeft(infoMap)((map,e) => expressionType(e,infoMap, identifiers))
				case AST.AssignmentExpression (e1,op,e2) => e1 match{
																case e1 : AST.Identifier => e2 match {															
																								case AST.AssignmentExpression (_,_,_) => expressionType(e2,infoMap,e1::identifiers)         
																								case _ => var updatedIdentifiers = e1::identifiers
																										var updatedMap = updatedIdentifiers.foldLeft(infoMap)((map,iden) => map+(iden -> List(e2)))
																										updatedIdentifiers.foldLeft(updatedMap)((map, iden) =>
																																	map.foldLeft(map){case (map1,(key,explist)) => 
																																								map1+(key -> explist.filter(exp => !isIdentifierInExpression(exp,iden)))})}	
																case _	=> infoMap} //The "_" case should not be used
				case AST.BinaryExpression (op,e1,e2) => expressionType(e2, expressionType (e1,infoMap, identifiers), identifiers)
				case AST.UnaryExpression (op, e) => expressionType(e,infoMap, identifiers)							
				case AST.PostfixExpression (op,e) => expressionType(e,infoMap, identifiers)					
				case AST.ConditionalExpression (e1,e2,e3) => expressionType(e3,expressionType(e2, expressionType (e1,infoMap, identifiers), identifiers), identifiers)
				case AST.CallExpression (e,oes) => expressionType(e, oes match {
																		case Some (es) => es.foldLeft(infoMap)((map,e) => expressionType(e,infoMap, identifiers))
																		case None => infoMap}, identifiers)	
				case AST.AllocationExpression (e) => expressionType(e,infoMap, identifiers)
				case AST.ArrayAccessExpression (e1,e2) => expressionType(e2, expressionType (e1,infoMap, identifiers), identifiers)
				case AST.ObjectAccessExpression (e,i) => expressionType(e,infoMap, identifiers)
				case AST.ArrayLiteral (oes) => oes match {
												case Some (es) => es.foldLeft(infoMap)((map,e) => expressionType(e,infoMap, identifiers))
												case None => infoMap}
				case _ => infoMap	
			}
		}
		
		node match {
			case CFG.Merge(_,_) => info_in.foldLeft(lattice.getBottom)((map, e) => lattice.getLub(map,e))
			case CFG.Continue(oe,_) => oe match {				
											case Some (e) => expressionType(e,info_in.head, List())
											case None => info_in.head}
			case CFG.Return(oe,_) => oe match {				
											case Some (e) => expressionType(e,info_in.head, List())
											case None => info_in.head}
			case CFG.Expression(e,_) => expressionType(e,info_in.head, List())
			case CFG.Assignment(i,oe,_) => oe match {															
											case Some (e) => e match {
																case AST.AssignmentExpression (_,_,_) => expressionType(e,info_in.head,List(i))         
																case _ => var updatedMap = info_in.head+(i -> List(e))
																					updatedMap.foldLeft(updatedMap){case (map,(key,explist)) => map+(key -> explist.filter(exp => !isIdentifierInExpression(exp,i)))}}
											case None => info_in.head}
			case CFG.If(e,_) => expressionType(e,info_in.head, List())
			case CFG.DoWhile(e,_) => expressionType(e,info_in.head, List())
			case CFG.ForIn(e1,e2,_) => e1 match {
											case e1 : AST.Expression => expressionType(e2, expressionType (e1,info_in.head, List()), List())
											case _ => expressionType (e2, info_in.head, List())
			}
			case CFG.With(e,_) => expressionType(e,info_in.head, List())
			case CFG.Switch(e,_) => expressionType(e,info_in.head, List())
			case CFG.CaseClause(e,_) => expressionType(e,info_in.head, List())
			case _ => info_in.head
		}		
	  }
	  
	  def isIdentifierInExpression(exp : AST.Expression, iden : AST.Identifier) : Boolean = {
		exp match {
			case AST.ExpressionList(es) => es.foldLeft(false)((bool, exp) => bool || isIdentifierInExpression(exp,iden))
			case AST.AssignmentExpression(e1,op,e2) => isIdentifierInExpression(e1,iden) || isIdentifierInExpression(e2,iden)
			case AST.BinaryExpression(op,e1,e2) => isIdentifierInExpression(e1,iden) || isIdentifierInExpression(e2,iden)
			case AST.UnaryExpression(op,e) => isIdentifierInExpression(e,iden)
			case AST.PostfixExpression(op,e) => isIdentifierInExpression(e,iden)
			case AST.ConditionalExpression(e1,e2,e3) => isIdentifierInExpression(e1,iden) || isIdentifierInExpression(e2,iden) || isIdentifierInExpression(e3,iden)
			case AST.CallExpression(e,oes) => isIdentifierInExpression(e,iden) || (oes match {
																					case Some (es) => es.foldLeft(false)((bool, exp) => bool || isIdentifierInExpression(exp,iden))
																					case None => false})
			case AST.AllocationExpression(e) => isIdentifierInExpression(e,iden)
			case AST.ArrayAccessExpression(e1,e2) => isIdentifierInExpression(e1,iden) || isIdentifierInExpression(e2,iden)
			case AST.ObjectAccessExpression(e,i) => isIdentifierInExpression(e,iden)
			case AST.ArrayLiteral(oes) => oes match {
											case Some (es) => es.foldLeft(false)((bool, exp) => bool || isIdentifierInExpression(exp,iden))
											case None => false}
			case AST.Identifier(s) => exp==iden		// May have to compare on the string
			case _ => false
		}
	  }  
	}
}