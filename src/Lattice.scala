package JSAnalyzer

	object Lattice {
	abstract class AbstractLattice[t] {
	   	  
	  def getBottom : t
	  
	  def getTop : t
	   
	  def compareElements(e1:t,e2:t) : Option[Boolean]
	  
	  def getLub(e1:t, e2:t) : t
	  
	  def getGlb(e1:t, e2:t) : t
	
	}

	class CSELattice(bottom : Map[AST.Identifier, List[AST.Expression]], top: Map[AST.Identifier, List[AST.Expression]]) extends Lattice.AbstractLattice[Map[AST.Identifier, List[AST.Expression]]] {
			  
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

	
}