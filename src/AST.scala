package JSAnalyzer

import scala.util.parsing.input.Positional

object AST {  
	def printOptList(ols : Option[List[Any]], glue: String = "") = ols match {
		case Some(ls) => printList(ls,glue)
		case None => ""
	}

	def printList(ls : List[Any], glue: String = "") = ls.headOption match {
	 	case Some(n) => ls.drop(1).foldLeft(n.toString())((str,n) => str + glue + n.toString() )
	 	case None => ""
	}

    abstract class ASTNode() extends Positional {
      def graphPrint(printer: ASTGrapher.Graph, level: Int) = new ASTGrapher.GraphNode("", List(("line", pos.line.toString())),1, 0)
      def graphLink(printer: ASTGrapher.Graph, level: Int, from: ASTGrapher.GraphNode, to: ASTNode, label:String) = {
        val graphTo = to.graphPrint(printer, level+1)
        printer.addLink(from, graphTo, label)
      }
      def graphList(printer: ASTGrapher.Graph, level: Int, from: ASTGrapher.GraphNode, to: List[ASTNode], label:String) = {
        val graphNodes = to map (x => { x.graphPrint(printer, level+1) })
        printer.addLink(from, printer.makeList(graphNodes), label)
      }
    }
  
	abstract class Expression() extends SourceElement 
	case class ExpressionList(lst: List[Expression]) extends Expression {
		override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
			val n = printer.addNode("ExpressionList", List(("line", pos.line.toString())), level)
			graphList(printer, level, n, lst, "lst")
			n
		}
	
	 	override def toString() = printList(lst, ", ")
	}


	case class AssignmentExpression(lhs: Expression, op: String, rhs: Expression) extends Expression {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("AssignmentExpression", List(("op", op), ("line", pos.line.toString())), level)
	    graphLink(printer, level, n, lhs, "lhs")
	    graphLink(printer, level, n, rhs, "rhs")
	    n
	  }

	  override def toString() = lhs.toString() + " " + op + "= " + rhs.toString()
	}

	case class FunctionExpression(name: Option[Identifier], params: Option[List[Identifier]], body: Block) extends Expression {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("FunctionExpression", List(("line", pos.line.toString())), level)
	    name.foreach(graphLink(printer, level, n, _, "name"))
	    params.foreach(graphList(printer, level, n, _, "params"))
	    graphLink(printer, level, n, body, "body")
	    n
	  }

	  override def toString() = {
	  	"function " + name.foreach((x) => x.toString()) + "("+ printOptList(params,",")+")" + body.toString()
	  }
	}

	case class BinaryExpression(op: String, t1: Expression, t2: Expression) extends Expression {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("BinaryExpression", List(("op", op), ("line", pos.line.toString())), level)
	    graphLink(printer, level, n, t1, "t1")
	    graphLink(printer, level, n, t2, "t2")
	    n
	  }

	  override def toString() = t1.toString() +" " + op + " "+ t2.toString()
	}
	case class UnaryExpression(op: String, t1: Expression) extends Expression {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("UnaryExpression", List(("op", op), ("line", pos.line.toString())), level)
	    graphLink(printer, level, n, t1, "t1")
	    n
	  }

	  override def toString() =  op + t1.toString()
	}
	case class PostfixExpression(op: String, t1: Expression) extends Expression {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("PostfixExpression", List(("op", op), ("line", pos.line.toString())), level)
	    graphLink(printer, level, n, t1, "t1")
	    n
	  }

	  override def toString() = t1.toString() + op

	}
	case class ConditionalExpression(cond: Expression, ifBranch: Expression, elseBranch: Expression) extends Expression {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("ConditionalExpression", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, cond, "condition")
	    graphLink(printer, level, n, ifBranch, "ifBranch")
	    graphLink(printer, level, n, ifBranch, "elseBranch")
	    n
	  }

	  override def toString() = "( " + cond.toString() + " ? " + ifBranch.toString() + " : " + elseBranch.toString() + " )"
	}
	case class CallExpression(callable: Expression, args: Option[List[Expression]])	extends Expression {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("CallExpression", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, callable, "callable")
	    args.foreach(graphList(printer, level, n, _, "args"))
	    n
	  }

	  override def toString() = callable.toString() + "("+printOptList(args,",") +")"
	}

	case class AllocationExpression(exp: Expression) extends Expression {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("AllocationExpression", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, exp, "exp")
	    n
	  }

	  override def toString() = "new " + exp.toString()
	}
	
	abstract class MemberAccessExpression() extends Expression
	case class ArrayAccessExpression(array: Expression, member: Expression) extends MemberAccessExpression {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("ArrayAccessExpression", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, array, "array")
	    graphLink(printer, level, n, member, "member")
	    n
	  }

	  override def toString() = array.toString() + "["+member.toString()+"]"
	}
	case class ObjectAccessExpression(obj: Expression, member: Identifier) extends MemberAccessExpression {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("ObjectAccessExpression", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, obj, "obj")
	    graphLink(printer, level, n, member, "member")
	    n
	  }

	  override def toString() = obj.toString() + "."+member.toString()
	}

	abstract class Statement() extends SourceElement
	
	/*
	case class FunctionDeclaration(name: Identifier, params: Option[List[Identifier]], body: Block) extends SourceElement {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("FunctionDeclaration", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, name, "name")
	    graphLink(printer, level, n, body, "body")
	    params.foreach(graphList(printer, level, n, _, "params"))
	    n
	  }

	  override def toString() = "function " + name.toString() + "("+printOptList(params,",") +") " + body.toString()
	} */

	abstract class Literal() extends Expression
	
	case class ArrayLiteral(value: Option[List[Expression]]) extends Literal {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("ArrayLiteral", List(("line", pos.line.toString())), level)
	    value.foreach(graphList(printer, level, n, _, "value"))
	    n
	  }

	  override def toString() = "["+ printOptList(value,",") +"]"
	}
	case class ObjectLiteral(value: Option[List[KVPair]]) extends Literal {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("ObjectLiteral", List(("line", pos.line.toString())), level)
	    value.foreach(graphList(printer, level, n, _, "value"))
	    n
	  }
	  override def toString() = "{"+ printOptList(value,",") +"}"
	}
	case class This() extends Literal {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("This", List(("line", pos.line.toString())), level)
	    n
	  }

	  override def toString() = "this"
 	}
	case class DecimalLiteral(value: String) extends Literal {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("DecimalLiteral", List(("value", value), ("line", pos.line.toString())), level)
	    n
	  }

	  override def toString() = value
	}
    case class HexIntegerLiteral(value: String) extends Literal {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("HexIntegerLiteral", List(("value", value), ("line", pos.line.toString())), level)
	    n
	  }
	  override def toString() = value
	}
    case class StringLiteral(value: String) extends Literal {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("StringLiteral", List(("value", value.stripPrefix("\"").stripSuffix("\"")), ("line", pos.line.toString())), level)
	    n
	  }
	  override def toString() = value
	}
    case class BooleanLiteral(value: Boolean) extends Literal {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("BooleanLiteral", List(("value", value.toString()), ("line", pos.line.toString())), level)
	    n
	  }
	  override def toString() = if (value) "true" else "false"
	}
    case class NullLiteral() extends Literal {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("NullLiteral", List(("value", "null"), ("line", pos.line.toString())), level)
	    n
	  }
	  override def toString() = "null"
	}
    
    case class Undefined() extends Expression {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("Undefined", List(("value", "undefined"), ("line", pos.line.toString())), level)
	    n
	  }
	  override def toString() = "undefined"
	}

	case class Identifier(value: String) extends Expression {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("Identifier", List(("id", value), ("line", pos.line.toString())), level)
	    n
	  }

	  override def toString() = value
	}
	
	//From page 26 at ecma262
	case class Block(sl : Option[List[Statement]]) extends Statement {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("Block", List(("line", pos.line.toString())), level)
	    sl.foreach(graphList(printer, level, n, _, "stmts"))
	    n
	  }

	  override def toString() = "{"+ printOptList(sl) +"}"
	}

	case class VariableStatement(vds: List[VariableDeclaration]) extends Statement {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("VariableStatement", List(("line", pos.line.toString())), level)
	    graphList(printer, level, n, vds, "decls")
	    n
	  }

	  override def toString() = "var " + printList(vds,", ")+";"
	}
	case class EmptyStatement() extends Statement {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("EmptyStatement", List(("line", pos.line.toString())), level)
	    n
	  }
	  override def toString() = "; "
	}
	case class ExpressionStatement(e:Expression) extends Statement {  
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("ExpressionStatement", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, e, "expr")
	    n
	  }
	  override def toString() = e.toString() + ";"
	}

	case class IfStatement(e:Expression,s1: Statement,s2:Option[Statement]) extends Statement {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("IfStatement", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, e, "cond")
	    graphLink(printer, level, n, s1, "ifBranch")
	    s2.foreach(graphLink(printer, level, n, _, "elseBranch"))
	    n
	  }

	  override def toString() = {
	  	val res = "if (" + e.toString() + ")" + s1.toString()
	  	s2 match {
	  		case None => res
	  		case Some(s2) => res + " else " + s2.toString()
	  	}
	  }
	}
	case class WhileStatement(e:Expression, s:Statement) extends Statement {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("WhileStatement", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, e, "cond")
	    graphLink(printer, level, n, s, "block")
	    n
	  }

	  override def toString() = "while ("+e.toString()+")" + s.toString()
	}
	case class DoWhileStatement(e: Expression, s:Statement) extends Statement {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("DoWhileStatement", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, e, "cond")
	    graphLink(printer, level, n, s, "block")
	    n
	  }

	  override def toString() = "do " + s.toString() + " while ("+e.toString()+");" 
	}
	case class ForStatement(e1:Option[ASTNode],e2:Option[Expression],e3:Option[Expression],s:Statement) extends Statement {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("ForStatement", List(("line", pos.line.toString())), level)
	    e1.foreach(graphLink(printer, level, n, _, "init"))
	    e2.foreach(graphLink(printer, level, n, _, "cond"))
	    e3.foreach(graphLink(printer, level, n, _, "step"))
	    graphLink(printer, level, n, s, "block")
	    n
	  }

	  override def toString() = {
	  	var res = "for ("
	  	res += (e1 match {
	  		case None => "; "
	  		case Some(e1) => e1 match {
	  			case e1 : Statement => e1.toString() //No ";" if its a statement
	  			case e1 : Expression => e1.toString() + "; "
	  			case _ => e1.toString() + "; "
	  		}
	  	})
	  	res += (e2 match {
	  		case None => ";"
	  		case Some(e2) => e2.toString() + "; "
	  	})
	  	res += (e3 match {
	  		case None => ""
	  		case Some(e3) => e3.toString()
	  	})
	  	res + ") " + s.toString()
	  }
	}

	case class ForInStatement(e1:ASTNode, e2:Expression, s:Statement) extends Statement {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("ForInStatement", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, e1, "member")
	    graphLink(printer, level, n, e2, "iterator")
	    graphLink(printer, level, n, s, "block")
	    n
	  }

	  override def toString() = "for ("+e1.toString()+" in "+e2.toString()+") "+s.toString()
	}
	case class ContinueStatement(i:Option[Identifier]) extends Statement {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("ContinueStatement", List(("line", pos.line.toString())), level)
	    i.foreach(graphLink(printer, level, n, _, "i"))
	    n
	  }

	  override def toString() = "continue;"

	}
	case class BreakStatement(i:Option[Identifier]) extends Statement {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("BreakStatement", List(("line", pos.line.toString())), level)
	    i.foreach(graphLink(printer, level, n, _, "i"))
	    n
	  }

	  override def toString() = "break;"
	}
	case class ReturnStatement(e: Option[Expression]) extends Statement  {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("ReturnStatement", List(("line", pos.line.toString())), level)
	    e.foreach(graphLink(printer, level, n, _, "value"))
	    n
	  }

	  override def toString() = e match {
	  	case None => "return;"
	  	case Some(e) => "return "+e+";"
	  }
	}
	case class WithStatement(e:Expression, s: Statement) extends Statement {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("WithStatement", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, e, "expr")
	    graphLink(printer, level, n, s, "block")
	    n
	  }

	  override def toString() = "with("+e.toString()+") " + s.toString()
	}
	case class LabelledStatement(i:Identifier, s: Statement) extends Statement	 {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("LabelledStatement", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, i, "id")
	    graphLink(printer, level, n, s, "block")
	    n
	  }

	  override def toString() = i.toString() + ": "+ s.toString()
	}
	case class SwitchStatement(e:Expression,cb:CaseBlock) extends Statement {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("SwitchStatement", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, e, "expr")
	    graphLink(printer, level, n, cb, "caseBlock")
	    n
	  }

	  override def toString() = "switch ("+e.toString()+") " + cb.toString()
	}
	case class ThrowStatement(e:Expression) extends Statement {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("ThrowStatement", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, e, "e")
	    n
	  }

	  override def toString() = "throw " + e.toString() + ";"
	}
	case class TryStatement(b:Block,c:Option[Catch], f:Option[Block]) extends Statement {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("TryStatement", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, b, "tryBlock")
	    c.foreach(graphLink(printer, level, n, _, "catchBlock"))
	    f.foreach(graphLink(printer, level, n, _, "finallyBlock"))
	    n
	  }

	  override def toString() = (c,f) match {
	  	case (Some(c),None) => "try "+ b.toString() + c.toString()
	  	case (Some(c),Some(f)) => "try " + b.toString() + c.toString() + f.toString()
	  	case (None,Some(f)) => "try " + b.toString() + f.toString()
	  	case (None,None) => "try " + b.toString()   //This should not happen
	  }
	}
	case class DebuggerStatement() extends Statement {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("DebuggerStatement", List(("line", pos.line.toString())), level)
	    n
	  }

	  override def toString() = "debugger;"
	}
	case class ImportStatement(names: List[String], wildcard: Boolean) extends Statement {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("ImportStatement", List(("name", names mkString "."), ("wildcard", wildcard.toString()), ("line", pos.line.toString())), level)
	    n
	  }

	  override def toString() = "IMPORT (NOT DEFINED)"  
	}

	//Helper classes for statements
	case class VariableDeclaration(i:Identifier,a:Option[Expression]) extends ASTNode {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("VariableDeclaration", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, i, "name")
	    a.foreach(graphLink(printer, level, n, _, "value"))
	    n
	  }

	  override def toString() =  i + (a match {
	  	case Some(e) => " = " + e.toString()
	  	case None => ""
	  })
	}
	case class CaseBlock(ccs:List[Case]) extends ASTNode {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("CaseBlock", List(("line", pos.line.toString())), level)
	    graphList(printer, level, n, ccs, "cases")
	    n
	  }

	  override def toString() = "{ "+printList(ccs) +" }"
	}

	abstract class Case() extends ASTNode 

	case class CaseClause(e:Expression,ss: Option[List[Statement]]) extends Case {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("CaseClause", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, e, "expr")
	    ss.foreach(graphList(printer, level, n, _, "stmts"))
	    n
	  }

	  override def toString() = "case " + e.toString() +": " + printOptList(ss)
	}

	case class DefaultClause(ss: Option[List[Statement]]) extends Case {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("DefaultClause", List(("line", pos.line.toString())), level)
	    ss.foreach(graphList(printer, level, n, _, "stmts"))
	    n
	  }

	  override def toString() = "default: " + printOptList(ss)
	}

	case class Catch(i:Identifier,b:Block) extends ASTNode {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("Catch", List(("line", pos.line.toString())), level)
	    graphLink(printer, level, n, i, "id")
	    graphLink(printer, level, n, b, "block")
	    n
	  }

	  override def toString() = "catch ("+i.toString()+") " + b.toString()
	}
	
	case class KVPair(key: ASTNode, value:ASTNode) extends ASTNode {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = key.graphPrint(printer, level)
	    graphLink(printer, level, n, value, "value")
	    n
	  }

	  override def toString() = key.toString() + ": " + value.toString()
	}

	//From page 101 at ecma262
	abstract class SourceElement() extends ASTNode

	case class Program(a:Option[List[SourceElement]]) extends ASTNode {
	  override def graphPrint(printer: ASTGrapher.Graph, level:Int) = {
	    val n = printer.addNode("Program", List(("line", pos.line.toString())), level)
	    a.foreach(graphList(printer, level, n, _, "elems"))
	    n
	  }

	  override def toString() = a match {
	  	case Some(se) => se.foldLeft("")((s,res) => s.toString() + res)
	  	case None => ""
	  }
	}
}

object ASTGrapher {


	class GraphLink(val from: GraphNode, val to: GraphNode, val label: String)
	class GraphNode(val name: String, val attributes: List[(String, String)], val id: Int, val level: Int) {
		def idString() = "n%d".format(id)
		def labelString() = {
			var res = ""
			if (attributes.length > 0) {
				res = "{%s".format(name)
				for ((attr, value) <- attributes) {
					res += "|{%s|%s}".format(attr, GraphvizDrawer.escape(value).stripPrefix("\"").stripSuffix("\""))
				}
				res += "}"
			} else {
				res = name
			}
			res
		}
	}

	class Graph() extends GraphvizDrawer.Graph {
		var nodeList: List[GraphNode] = List()
		var edgeList: List[GraphLink] = List()
		var counter = 0

		override def name() = "ASTGraph"

		override def nodes() : List[GraphvizDrawer.Node] = this.nodeList.foldLeft(List() : List[GraphvizDrawer.Node])((list,node) => {
				GraphvizDrawer.Node(node.idString(),node.labelString(),None,Some(node.level)) :: list
			})

		override def edges() : List[GraphvizDrawer.Edge] = this.edgeList.foldLeft(List() : List[GraphvizDrawer.Edge])((list,edge) => {
				GraphvizDrawer.Edge(edge.from.idString(), edge.to.idString(), Some(edge.label)) :: list
			})

		override def subgraphs() : List[GraphvizDrawer.Graph] = List()

		def addNode(name: String, attributes: List[(String, String)], level: Int) = {
			val node = new GraphNode(name, attributes, counter, level)
			nodeList = node :: nodeList
			counter += 1
			node
		}


		/*
		* Create a link from @from to @to with label @label
		*/
		def addLink(from: GraphNode, to: GraphNode, label: String) = {
			val link = new GraphLink(from, to, label)
			edgeList = link :: edgeList
		}

	  /*
	   * Given a list of graph nodes, create the necessary links between them
	   * and return the first node
	   */
		def makeList(lst: List[GraphNode]) = {
			var prevNode: Option[GraphNode] = None
			var firstNode: Option[GraphNode] = None

			for (node <- lst) {
				firstNode match {
					case None => { firstNode = Some(node) }
					case Some(x) => {} // nop
				}
				prevNode match {
					case None => { prevNode = Some(node) }
					case Some(prev) => { addLink(prev, node, ""); prevNode = Some(node) }
				}
			}

			firstNode.get
		}
	}

	
	def getGraph(start: AST.ASTNode) : GraphvizDrawer.Graph = {
		var graph = new Graph()
		start.graphPrint(graph,0)
		graph
	}
}