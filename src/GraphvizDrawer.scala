package JSAnalyzer

object GraphvizDrawer {
	var tab = "\t"
	def export(graph: Graph ,export:  java.io.PrintStream = System.out) = {
 		export.println("digraph "+graph.name()+" {")

 		var ranks = graph.nodes().foldLeft(Map() : Map[Int, List[Node]])((ranks,node) => {
 			export.println( tab + "\"%s\" [shape=%s label=\"%s\"];".format(node.id, node.shape.getOrElse(Record()), node.label) )
 			node.rank match {
 				case Some(rank) => ranks + ((rank,node :: ranks.getOrElse(rank,List())))
 				case None => ranks
 			}
 		})

 		graph.edges().foreach((edge) => edge.label match {
 			case Some(l) => export.println(tab + "\"%s\" -> \"%s\" [label=\"%s\"];".format(edge.from,edge.to, l))
 			case None => export.println(tab + "\"%s\" -> \"%s\"".format(edge.from, edge.to))
 		})

 		ranks.foreach{
	      case (key, value) => export.println(tab + "{ rank=same; %s }".format(value.map(_.id).reduceLeft(_ + " " + _)))
	    }


 		export.println("}")
	}

	def escape(s :String) : String = {
		s.map(_ match { 
			  case '\n' => "\\n"
	          case '\'' => "&apos;"
	          case '"' => "&quot;"
	          case '<' => "&lt;"
	          case '>' => "&gt;"
	          case '{' => "\\{"
	          case '}' => "\\}"
	          case ']' => "\\]"
	          case '[' => "\\["
	          case other => other toString
        }) mkString;
	}


	abstract class Shape() {
		override def toString() : String = {
			var n = this 
			n match {
				case n : Diamond => "Mdiamond"
				case n : Square => "Msquare"
				case n : Record => "record"
			}
		}
	}
	case class Diamond() extends Shape
	case class Square() extends Shape
	case class Record() extends Shape

	case class Node(id : String, label: String, shape: Option[Shape] = None, rank: Option[Int] = None)

	case class Edge(from : String, to: String, label: Option[String] = None)

	abstract class Graph() {
		def name() : String
		def edges() : List[Edge]
		def nodes() : List[Node]
	}
}



