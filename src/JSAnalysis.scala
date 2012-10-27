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

	def analyze(file: File) = {
		println("Analyze: " + file.toString())
	}

	def graphviz(cfg : CFG.ControlFlowGraph, filename : String, dir: String) = {
		GraphvizExporter.export("filename",cfg, new PrintStream(dir + "/"+ filename+".dot"))
		Runtime.getRuntime().exec("dot -Tgif -o "+dir + "/"+ filename+".gif " + dir + "/"+ filename+".dot")
	}

	def main(args : Array[String]) = {
		var cfg1 = ControlFlow.emptyCFG() + CFG.EmptyNode()
		args.foreach((f) => {
			var file = new File(f)
			if (file.isFile()){
				analyze(file)
			} else if (file.isDirectory()) {	
				analyzeDir(file)
			} else {
				System.err.println("Couldn't read: " + f)
			}
		})
	}
}