import JSAnalyzer._
import java.io._
import JSAnalyzer.DefUseChain

object JSAnalysis {
	case class RuntimeOpts(
			printAst: Boolean = false, 
			graphAst: Boolean = false, 
			graphCfg: Boolean = false, 
			graphCSE: Boolean = false, 
			graphLiveness: Boolean = false, 
			graphCall: Boolean = false, 
			graphDom: Boolean = false,
			graphDefUse: Boolean = false,
		 	files: List[String] = List()) {
		override def toString() = {
			"" +(if (printAst) "printAst " else "") + 
				(if (graphAst) "graphAst " else "") + 
				(if (graphCfg) "graphCfg " else "") + 
				(if (graphCSE) "graphCSE " else "") +
				(if (graphLiveness) "graphLiveness " else "") +
				(if (graphCall) "graphCall " else "") + 
				(if (graphDom) "graphDom " else "") +
				(if (graphDefUse) "graphDefUse " else "")
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
	    var bcfg = ControlFlow.toBlocked(cfg)
	    var rcfg = ControlFlow.reverse( cfg )
	    var callFG = CallFG.createCFG( cfg )

		if (opts.graphCfg) graphCFG(cfg, filename, dir)

		if (opts.graphCSE) graphCSE(cfg, filename, dir)

		if (opts.graphLiveness) graphLiveness(cfg, rcfg, filename, dir)

		if (opts.graphCall) graphCall(callFG, filename,dir)

		if (opts.graphDom) graphDom(bcfg, filename, dir)
		
		if (opts.graphDefUse) graphDefUse(cfg, bcfg, filename, dir)
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

	def graphCall(cfg : CallFG.CallFlowGraph, filename : String, dir: String) : Unit = {
		GraphvizDrawer.export(CallFG.graph("CallFlowGraph",cfg), new PrintStream(dir + filename+".call.dot"))
		Runtime.getRuntime().exec("dot -Tgif -o "+dir + filename+".call.gif " + dir + filename+".call.dot")
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
	
	def graphDom(cfg : CFG.ControlFlowGraph, filename : String, dir: String) : Unit = {
	}
	
	def graphDefUse(cfg : CFG.ControlFlowGraph, bcfg : CFG.ControlFlowGraph, filename : String, dir: String) : Unit = {
	  val reachingDefs = new DefUseChain.ReachingDefs(cfg)
	  val analysis = DataFlowAnalysis.worklistalgorithm(reachingDefs, cfg)
	  val defUseChain = reachingDefs.useDefChain(cfg, analysis)
	  
	  val domAnalysis = new Dominance.DominanceConstructor(bcfg)
	  val dominance = domAnalysis.dom()
	  val idom = domAnalysis.idom(dominance)

	  val df = domAnalysis.domFront(idom)
	  
	  val domTree = Dominance.makeGraph(bcfg, idom - bcfg.start)

	  GraphvizDrawer.export(CFGGrapher.graph("Dom", Dominance.makeGraph(bcfg, dominance)), new PrintStream(dir + filename+".dom.dot"))
	  Runtime.getRuntime().exec("dot -Tgif -o "+dir + filename+".dom.gif " + dir + filename+".dom.dot")
		
	  GraphvizDrawer.export(CFGGrapher.graph("IDom", domTree), new PrintStream(dir + filename+".idom.dot"))
	  Runtime.getRuntime().exec("dot -Tgif -o "+dir + filename+".idom.gif " + dir + filename+".idom.dot")
	  
	  GraphvizDrawer.export(CFGGrapher.graph("Def-Use chain", DefUseChain.makeGraph(cfg, defUseChain)), new PrintStream(dir + filename+".duc.dot"))
	  Runtime.getRuntime().exec("dot -Tgif -o "+dir + filename+".duc.gif " + dir + filename+".duc.dot")
	  
	  val nbcfg = SSA(bcfg, defUseChain, domTree, df)
	  GraphvizDrawer.export(CFGGrapher.graph("Phi", nbcfg), new PrintStream(dir + filename+".phi.dot"))
	  Runtime.getRuntime().exec("dot -Tgif -o "+dir + filename+".phi.gif " + dir + filename+".phi.dot")
	}
	
	def main(args : Array[String]) = {
		val opts = args.foldLeft(RuntimeOpts())((opts,arg) => arg match {
			case "-print-ast" => RuntimeOpts(true, opts.graphAst, opts.graphCfg, opts.graphCSE, opts.graphLiveness, opts.graphCall, opts.graphDom, opts.graphDefUse, opts.files)
			case "-graph-ast" => RuntimeOpts(opts.printAst, true, opts.graphCfg, opts.graphCSE, opts.graphLiveness, opts.graphCall, opts.graphDom, opts.graphDefUse, opts.files)
			case "-graph-cfg" => RuntimeOpts(opts.printAst, opts.printAst, true, opts.graphCSE, opts.graphLiveness, opts.graphCall, opts.graphDom, opts.graphDefUse, opts.files)
			case "-graph-cse" => RuntimeOpts(opts.printAst, opts.printAst, opts.graphCfg, true, opts.graphLiveness, opts.graphCall, opts.graphDom, opts.graphDefUse, opts.files)
			case "-graph-liveness" => RuntimeOpts(opts.printAst, opts.printAst, opts.graphCfg, opts.graphCSE, true, opts.graphCall, opts.graphDom, opts.graphDefUse, opts.files)
			case "-graph-call" => RuntimeOpts(opts.printAst, opts.printAst, opts.graphCfg, opts.graphCSE, opts.graphLiveness, true, opts.graphDom, opts.graphDefUse, opts.files)
			case "-graph-dom" => RuntimeOpts(opts.printAst, opts.printAst, opts.graphCfg, opts.graphCSE, opts.graphLiveness, opts.graphCall, true, opts.graphDefUse, opts.files)
			case "-graph-defuse" => RuntimeOpts(opts.printAst, opts.printAst, opts.graphCfg, opts.graphCSE, opts.graphLiveness, opts.graphCall, opts.graphDom, true, opts.files)
			case _ => RuntimeOpts(opts.printAst, opts.graphAst, opts.graphCfg, opts.graphCSE, opts.graphLiveness, opts.graphCall, opts.graphDom, opts.graphDefUse, arg :: opts.files)
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