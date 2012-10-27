import JSAnalyzer._
import java.io._

object JSAnalysis {
	def analyzeDir(dir : File) : Unit = {
		dir.listFiles().foreach((file) => {
			if (file.isFile()){
				analyze(file)
			} else if (file.isDirectory()) {	
				analyzeDir(file)
			}
		})
	}

	def analyze(file: File) : Unit = {
		println("Analyze: " + file.toString())
	}

	def analyze(files : Array[String]) : Unit = {
		files.foreach((f) => {
			var file = new File(f)
			if (file.isFile()){
				analyze(file)
			} else if (file.isDirectory()) {	
				analyzeDir(file)
			} else {
				System.err.println("Couldn't analyze: " + f)
			}
		})
	}

	def graphviz(cfg : CFG.ControlFlowGraph, filename : String, dir: String) = {
		GraphvizExporter.export(filename,cfg, new PrintStream(dir + filename+".dot"))
		Runtime.getRuntime().exec("dot -Tgif -o "+dir + filename+".gif " + dir + filename+".dot")
	}

	def main(args : Array[String]) = {
		analyze(args)

		var cfg1 = List("edge1","edge2").foldLeft(ControlFlow.emptyCFG())((cfg,edge) => ControlFlow.append( cfg, CFG.EmptyNode(), Some(edge)))
		graphviz(cfg1, "test1", "")
		var cfg2 = List("edge4","edge5","edge6").foldLeft(ControlFlow.emptyCFG())((cfg,edge) => ControlFlow.append( cfg, CFG.EmptyNode(), Some(edge)))
		graphviz(cfg2, "test2", "")
		var cfg3 = List("edge7","edge8","edge9").foldLeft(ControlFlow.emptyCFG())((cfg,edge) => ControlFlow.append( cfg, CFG.EmptyNode(), Some(edge)))
		graphviz(cfg3, "test3", "")

		var cfg4 = ControlFlow.branchMerge(cfg1, List((cfg2,Some("CFG2")),(cfg3,Some("CFG3"))), CFG.Merge("Test Merge"))
		graphviz(cfg4, "test4", "")
	}
}