import JSAnalyzer._
import java.io._

object JSAnalysis {
	def main(args : Array[String]) {
		var cfg1 = ControlFlow.emptyCFG() + CFG.EmptyNode()
		GraphvizExporter.export("cfg1",cfg1,new PrintStream("cfg1.dot"))
		Runtime.getRuntime().exec("dot -Tgif cfg1.dot cfg1.gif");

		var cfg2 = ControlFlow.append( ControlFlow.emptyCFG(), CFG.EmptyNode(), Some("test1"))
		GraphvizExporter.export("cfg2",cfg2,new PrintStream("cfg2.dot"))
		Runtime.getRuntime().exec("dot -Tgif cfg2.dot cfg2.gif");

		var cfg3 = ControlFlow.emptyCFG() > CFG.EmptyNode()
		GraphvizExporter.export("cfg3",cfg3,new PrintStream("cfg3.dot"))
		Runtime.getRuntime().exec("dot -Tgif cfg3.dot cfg3.gif");

		var cfg4 = ControlFlow.prepend( ControlFlow.emptyCFG(), CFG.EmptyNode(), Some("test2"))
		GraphvizExporter.export("cfg4",cfg4,new PrintStream("cfg4.dot"))
		Runtime.getRuntime().exec("dot -Tgif cfg4.dot cfg4.gif");

		var cfg5 = cfg1 :: cfg4
		GraphvizExporter.export("cfg5",cfg5,new PrintStream("cfg5.dot"))
		Runtime.getRuntime().exec("dot -Tgif cfg5.dot cfg5.gif");

		var cfg6 = ControlFlow.branchMerge( cfg1, List((cfg2,Some("cfg2")),(cfg3,Some("cfg3")),(cfg4,Some("cfg4")),(cfg5,Some("cfg5"))), CFG.Merge("Test"))
		GraphvizExporter.export("cfg6",cfg6,new PrintStream("cfg6.dot"))
		Runtime.getRuntime().exec("dot -Tgif cfg6.dot cfg6.gif");
		

	}
}