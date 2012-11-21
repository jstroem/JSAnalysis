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
	
	class TestLattice(st : Set[Int], botm : Int, tp: Int) extends AbstractLattice[Int](st, botm, tp){
	  val bottom = botm;
	  val top = tp;
	  val set = st;
	  
	  def getBottom : Int = {
	    bottom;
	  }
	  
	  def getTop : Int = {
	    top;
	  }
	   
	  def compareElement(e1:Int,e2:Int) : Option[Boolean] = {
	    Some (e1>e2)
	  }
	  
	  def getLub(e1:Int, e2:Int) : Int = {
	    if(e2>e1){
	      e2
	    }else{
	      e1
	    }
	  }
	  
	  def getGlb(e1:Int, e2:Int) : Int = {
	    if(e1>e2){
	      e1
	    } else {
	      e2
	    }
	  }
	  
	}
}