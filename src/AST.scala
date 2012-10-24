object AST {
	abstract class Identifier //TODO: Create identifier

	abstract class Expression //TODO: Create Expression

	abstract class Statement extends SourceElement //TODO: create statement

	abstract class Operation //TODO: create Operation


	//From page 26 at ecma262
	case class Block(sl : List[Statement]) extends Statement

	case class VariableStatement() extends Statement
	case class EmptyStatement() extends Statement
	case class ExpressionStatement() extends Statement
	case class IfStatement(e:Expression,s1:Option[Statement],s2:Option[Statement]) extends Statement
	case class IterationStatement() extends Statement
	case class ContinueStatement() extends Statement
	case class BreakStatement() extends Statement
	case class ReturnStatement() extends Statement 
	case class WithStatement() extends Statement
	case class LabelledStatement() extends Statement
	case class SwitchStatement() extends Statement
	case class ThrowStatement() extends Statement
	case class TryStatement() extends Statement
	case class DebuggerStatement() extends Statement

	//case class PostfixExpression(e:LeftHandSideExpression,o:Operation) extends Expression


	//From page 101 at ecma262
	abstract class SourceElement

	case class FunctionDeclaration() extends SourceElement //TODO: create this

	case class SourceElements(se:SourceElement,ses:SourceElement)

	case class Program(a:Some[SourceElements])

}