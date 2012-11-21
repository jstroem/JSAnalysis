package JSAnalyzer

	object Lattice {
	abstract class AbstractLattice[t](botm : t, tp : t) {
	   	  
	  def getBottom : t
	  
	  def getTop : t
	   
	  def compareElement(e1:t,e2:t) : Option[Boolean]
	  
	  def getLub(e1:t, e2:t) : t
	  
	  def getGlb(e1:t, e2:t) : t
	
	}
	
}