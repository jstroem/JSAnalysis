import JSAnalyzer._
import java.io._

object JSAnalysis {
	case class RuntimeOpts(printAst: Boolean = false, graphAst: Boolean = false, graphCfg: Boolean = false, files: List[String] = List()) {
		override def toString() = "" + (if (printAst) "printAst " else "") + (if (graphAst) "graphAst " else "") + (if (graphCfg) "graphCfg " else "")
	}

	def analyzeDir(dir : File, opts : RuntimeOpts = RuntimeOpts()) : Unit = {
		dir.listFiles().foreach((file) => {
			if (file.isFile()){
				analyze(file, opts)
			} else if (file.isDirectory()) {	
				analyzeDir(file, opts)
			}
		})
	}

	def splitFilename(f : File) : (String,String) = {
		var dir = f.getParent() + "/"
		var file = f.getName()
		var idx = file.lastIndexOf('.')
		var extension = if (idx >= 0)  file.substring(idx) else ""
		var filename = if (idx >= 0) file.substring(0, idx) else file
		(dir,filename)
	}

	def analyze(file: File, opts : RuntimeOpts = RuntimeOpts()) : Unit = {
		println("Analyze: " + file.getName())
		var (dir,filename) = splitFilename(file)

		var ast = makeAst(file)
		if (opts.printAst) printAST(ast, new PrintStream(dir + filename + ".ast"))
		if (opts.graphAst) graphAST(ast, filename, dir)

		var cfg = ControlFlow.program( ast )
		if (opts.graphCfg) graphCFG(cfg, filename, dir)
	}

	def makeAst(file: File) : AST.Program = {
		val source = scala.io.Source.fromFile(file)
		val lines = source.mkString
		source.close()
		JSParser(lines)
	}

	def printAST(ast :AST.ASTNode, stream : PrintStream) : Unit = {
		stream.print(ast)
	}

	def graphAST(ast : AST.ASTNode, filename : String, dir : String) : Unit = {
		GraphvizDrawer.export(ASTGrapher.getGraph(ast), new PrintStream(dir + filename+".ast.dot"))
		Runtime.getRuntime().exec("dot -Tgif -o "+dir + filename+".ast.gif " + dir + filename+".ast.dot")
	}	

	def graphCFG(cfg : CFG.ControlFlowGraph, filename : String, dir: String) : Unit = {
		GraphvizDrawer.export(CFGGrapher.graph(cfg), new PrintStream(dir + filename+".cfg.dot"))
		Runtime.getRuntime().exec("dot -Tgif -o "+dir + filename+".cfg.gif " + dir + filename+".cfg.dot")
	}

	def main(args : Array[String]) = {
		val opts = args.foldLeft(RuntimeOpts())((opts,arg) => arg match {
			case "-print-ast" => RuntimeOpts(true, opts.graphAst, opts.graphCfg, opts.files)
			case "-graph-cfg" => RuntimeOpts(opts.printAst, opts.printAst, true, opts.files)
			case "-graph-ast" => RuntimeOpts(opts.printAst, true, opts.graphCfg, opts.files)
			case _ => RuntimeOpts(opts.printAst, opts.graphAst, opts.graphCfg, arg :: opts.files)
		})
		println("Running analysis with options: "+ opts.toString())

		opts.files.foreach((f) => {
			var file = new File(f)
			if (file.isFile()){
				analyze(file, opts)
			} else if (file.isDirectory()) {	
				analyzeDir(file, opts)
			} else {
				System.err.println("Couldn't analyze: " + f)
			}
		})
	}
}