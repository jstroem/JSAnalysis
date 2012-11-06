package JSAnalyzer

object Test {
  def main(args: Array[String]) {
			if (args.length != 1) {
				println("Please supply filename")
				System.exit(1)
			} else {
				val source = scala.io.Source.fromFile(args(0))
				val lines = source.mkString
				source.close()
				
				//println(lines)
				
				val ast = JSParser(lines)
				val printer = new GraphPrinter(ast)
				
				printer.print()
			}
		}
}