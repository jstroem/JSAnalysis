package JSAnalyzer

	object Lattice {
	abstract class AbstractLattice[t](set : Set[t], botm : t, tp : t) {
	   
	  def getElements : Set[t]={
	    return set;
	  }
	  
	  def getBottom : t
	  
	  def getTop : t
	   
	  def compareElement(e1:t,e2:t) : Option[Boolean]
	  
	  def getLub(e1:t, e2:t) : t
	  
	  def getGlb(e1:t, e2:t) : t
	
	}
	
}