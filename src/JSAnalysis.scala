import JSAnalyzer._
import java.io._

object JSAnalysis {
	case class RuntimeOpts(
			printAst: Boolean = false, 
			graphAst: Boolean = false, 
			graphCfg: Boolean = false, 
			graphCSE: Boolean = false, 
			graphLiveness: Boolean = false, 
		 	files: List[String] = List()) {
		override def toString() = {
			"" +(if (printAst) "printAst " else "") + 
				(if (graphAst) "graphAst " else "") + 
				(if (graphCfg) "graphCfg " else "") + 
				(if (graphCSE) "graphCSE " else "") +
				(if (graphLiveness) "graphLiveness " else "")
		}
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
	    var rcfg = ControlFlow.reverse( cfg )

		if (opts.graphCfg) graphCFG(cfg, filename, dir)

		if (opts.graphCSE) graphCSE(cfg, filename, dir)

		if (opts.graphLiveness) graphLiveness(cfg, rcfg, filename, dir)

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
		GraphvizDrawer.export(CFGGrapher.graph("ControlFlowGraph",cfg), new PrintStream(dir + filename+".cfg.dot"))
		Runtime.getRuntime().exec("dot -Tgif -o "+dir + filename+".cfg.gif " + dir + filename+".cfg.dot")
	}

	def graphCSE(cfg : CFG.ControlFlowGraph, filename : String, dir: String) : Unit = {
		var cseAnalyzer = new CommonSubExp.CommonSubExpAnalysis(CommonSubExp.getCSELatticeBottom(cfg),CommonSubExp.getCSELatticeTop(cfg))
		var cse = DataFlowAnalysis.worklistalgorithm(cseAnalyzer,cfg)
		var csfe = cfg.info.functions.foldLeft(Map() :  Map[AST.Identifier, Map[(CFG.ControlFlowNode, CFG.ControlFlowNode), Map[AST.Identifier, List[AST.Expression]]]])((map,pair) => {
			var (name,func) = pair
			var csfeAnalyzer = new CommonSubExp.CommonSubExpAnalysis(CommonSubExp.getCSELatticeBottom(func.cfg),CommonSubExp.getCSELatticeTop(func.cfg))
			map + ((func.name,DataFlowAnalysis.worklistalgorithm(csfeAnalyzer,func.cfg)))
		})
		GraphvizDrawer.export(CSEGrapher.graph("CSEFlowGraph", cfg, cse,csfe), new PrintStream(dir + filename+".cse.dot"))
		Runtime.getRuntime().exec("dot -Tgif -o "+dir + filename+".cse.gif " + dir + filename+".cse.dot")
	}

	def graphLiveness(cfg: CFG.ControlFlowGraph, rcfg: CFG.ControlFlowGraph, filename: String, dir: String ) : Unit = {
		var analysis = new Liveness.LivenessAnalysis(Liveness.variables(cfg))
		var live = DataFlowAnalysis.worklistalgorithm(analysis,rcfg);	
		var flive = rcfg.info.functions.foldLeft(Map() :  Map[AST.Identifier, Map[(CFG.ControlFlowNode, CFG.ControlFlowNode),List[AST.Identifier]]])((map,pair) => {
			var (name,func) = pair
			map + ((func.name,DataFlowAnalysis.worklistalgorithm(analysis,func.cfg)))
		})
		GraphvizDrawer.export(Liveness.graph("LivenessAnalysis", cfg, live, flive), new PrintStream(dir + filename+".live.dot"))
		Runtime.getRuntime().exec("dot -Tgif -o "+dir + filename+".live.gif " + dir + filename+".live.dot")
	}
	
	def main(args : Array[String]) = {
		val opts = args.foldLeft(RuntimeOpts())((opts,arg) => arg match {
			case "-print-ast" => RuntimeOpts(true, opts.graphAst, opts.graphCfg, opts.graphCSE, opts.graphLiveness, opts.files)
			case "-graph-ast" => RuntimeOpts(opts.printAst, true, opts.graphCfg, opts.graphCSE, opts.graphLiveness, opts.files)
			case "-graph-cfg" => RuntimeOpts(opts.printAst, opts.printAst, true, opts.graphCSE, opts.graphLiveness, opts.files)
			case "-graph-cse" => RuntimeOpts(opts.printAst, opts.printAst, opts.graphCfg, true, opts.graphLiveness, opts.files)
			case "-graph-liveness" => RuntimeOpts(opts.printAst, opts.printAst, opts.graphCfg, opts.graphCSE, true, opts.files)
			case _ => RuntimeOpts(opts.printAst, opts.graphAst, opts.graphCfg, opts.graphCSE, opts.graphLiveness, arg :: opts.files)
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