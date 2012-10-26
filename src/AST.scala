package JSAnalyzer

import scala.util.parsing.input.Positional

object AST {  
    abstract class ASTNode extends Positional
  
	abstract class Expression extends ASTNode //TODO: Create Expression
	case class LeftHandSideExpression extends Expression //TODO: create this
	case class AssignmentExpression extends Expression //TODO: create this
	case class FunctionExpression extends Expression 
	case class BinaryExpression(op: String, t1: Expression, t2: Expression) extends Expression
	case class UnaryExpression(op: String, t1: Expression) extends Expression

	abstract class Statement extends SourceElement //TODO: create statement
	case class FunctionDeclaration extends SourceElement //TODO: create this
	
	abstract class Literal extends Expression
	
	case class ArrayLiteral(value: List[AssignmentExpression]) extends Literal
	case class ObjectLiteral(value: List[(Expression, AssignmentExpression)]) extends Literal
	case class This extends Literal
	case class DecimalLiteral(value: String) extends Literal
    case class HexIntegerLiteral(value: String) extends Literal
    case class StringLiteral(value: String) extends Literal
    case class BooleanLiteral(value: Boolean) extends Literal
    case class NullLiteral extends Literal

	abstract class Operation //TODO: create Operation

	case class Identifier(value: String) extends Expression
	
	//From page 26 at ecma262
	case class Block(sl : Option[List[Statement]]) extends Statement
	case class VariableStatement(vds: List[VariableDeclaration]) extends Statement
	case class EmptyStatement extends Statement
	case class ExpressionStatement(e:Expression) extends Statement
	case class IfStatement(e:Expression,s1: Statement,s2:Option[Statement]) extends Statement
	case class WhileStatement(e:Expression, s:Statement) extends Statement
	case class DoWhileStatement extends Statement 				//TODO: Create this, Should we maybe leave this out
	case class ForStatement(e1:Option[Expression],e2:Option[Expression],e3:Option[Expression],s:Statement) extends Statement
	case class ForInStatement extends Statement 					//TODO: Create this, Should we maybe leave this out
	case class ContinueStatement(i:Option[Identifier]) extends Statement
	case class BreakStatement(i:Option[Identifier]) extends Statement
	case class ReturnStatement(e: Option[Expression]) extends Statement 
	case class WithStatement(e:Expression, s: Statement) extends Statement
	case class LabelledStatement(i:Identifier, s: Statement) extends Statement	
	case class SwitchStatement(e:Expression,cb:CaseBlock) extends Statement
	case class ThrowStatement(e:Expression) extends Statement
	case class TryStatement(b:Block,c:Option[Catch], f:Option[Finally]) extends Statement
	case class DebuggerStatement extends Statement

	//Helper classes for statements
	case class VariableDeclaration(i:Identifier,a:Option[AssignmentExpression])
	case class CaseBlock(ccs:List[CaseClause])
	case class CaseClause(e:Expression,ss: List[Statement])
	case class Catch(i:Identifier,b:Block)
	case class Finally(b:Block)


	//From page 101 at ecma262
	abstract class SourceElement extends ASTNode

	case class SourceElements(se:SourceElement,ses:SourceElement)

	case class Program(a:Some[SourceElements])

}