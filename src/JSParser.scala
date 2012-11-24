package JSAnalyzer

import scala.util.parsing.combinator.RegexParsers

object JSParser extends RegexParsers { 
  lazy val Keyword = "break" |
		  		"do" |
                "instanceof" |
                "typeof" |
                "case" |
                "else" |
                "new" |
                "var" |
                "catch" |
                "finally" |
                "return" |
                "void" |
                "continue" |
                "for" |
                "switch" |
                "while" |
                "debugger" |
                "function" |
                "this" |
                "with" |
                "default" |
                "if" |
                "throw" |
                "delete" |
                "in" |
                "try"
             ;
  
  lazy val FunctionBody = positioned("{" ~> opt(StatementList) <~ "}" ^^ { AST.Block(_) }
                   );

  lazy val BitwiseANDExpressionNoIn = positioned(EqualityExpressionNoIn * (BitwiseANDOperator ^^ makeBinaryOp)
                               );

  lazy val Elision = rep1(",") ^^ { _ map (a => AST.Undefined()) }
              ;

  lazy val Statement = positioned(Block |
                  //JScriptVarStatement |
                  VariableStatement |
                  EmptyStatement |
                  LabelledStatement |
                  ExpressionStatement |
                  IfStatement |
                  IterationStatement |
                  ContinueStatement |
                  BreakStatement |
                  ImportStatement |
                  ReturnStatement |
                  WithStatement |
                  SwitchStatement |
                  ThrowStatement |
                  TryStatement  
                );

  lazy val VariableDeclarationNoIn = positioned(Identifier ~ opt(InitialiserNoIn) ^^ { case id ~ i => AST.VariableDeclaration(id, i) }
                              );

  lazy val LogicalANDExpression = positioned(BitwiseORExpression * (LogicalANDOperator ^^ makeBinaryOp)
                           );

  lazy val ArgumentList = rep1sep(AssignmentExpression, ",")
                   ;

  lazy val LogicalOROperator = "||"
                        ;

  lazy val PostfixOperator = "++" | "--"
                      ;

  lazy val ExpressionStatement = positioned(Expression <~ opt(";") ^^ { AST.ExpressionStatement(_) }
                          );

  lazy val CaseClauses = rep1(CaseClause)
                  ;

  lazy val StatementList:Parser[List[AST.Statement]] = rep1(Statement) ^^ { _.map(x => {
    x match {
      case AST.ExpressionStatement(AST.ExpressionList(el)) => el.map(AST.ExpressionStatement(_))
      case _ => List(x)
    }
  }).reduceLeft((a ,b) => a ::: b) }
                    ;

  lazy val BitwiseORExpressionNoIn = positioned(BitwiseXORExpressionNoIn * (BitwiseOROperator ^^ makeBinaryOp)
                              );

  lazy val CaseBlock = ("{" ~> opt(CaseClauses)) ~ DefaultClause ~ (opt(CaseClauses) <~ "}") ^^ { case c1 ~ d ~ c2 => { AST.CaseBlock((c1.getOrElse(Nil) ::: List(d)) ++ c2.getOrElse(Nil)) } } |
                  "{" ~> opt(CaseClauses) <~ "}" ^^ { x => { AST.CaseBlock(x.getOrElse(Nil)) } }
                ;

  lazy val AssignmentOperator = "=" |
                           "*=" |
                           "/=" |
                           "%=" |
                           "+=" |
                           "-=" |
                           "<<=" |
                           ">>=" |
                           ">>>=" |
                           "&=" |
                           "^=" |
                           "|="  
                         ;

  lazy val FunctionExpression = positioned("function" ~> opt(Identifier) ~ ("(" ~> opt(FormalParameterList) <~ ")") ~ FunctionBody ^^ { case name ~ params ~ body => { AST.FunctionExpression(name, params, body) } }
                         );

  lazy val Finally = positioned("finally" ~> Block
              );

  lazy val SourceElement = positioned(FunctionDeclaration |
                      Statement  
                    );

  lazy val CaseClause = positioned("case" ~> Expression ~ (":" ~> opt(StatementList)) ^^ { case e ~ ss => { AST.CaseClause(e, ss) } }
                 );

  lazy val EmptyStatement = ";" ^^^ { AST.EmptyStatement() }
                     ;

  lazy val ReturnStatement = positioned("return" ~> opt(Expression) <~ opt(";") ^^ { AST.ReturnStatement(_) }
                      );

  lazy val PostfixExpression = positioned(LeftHandSideExpression ~ PostfixOperator ^^ { case e ~ op => { AST.PostfixExpression(op, e) } } |
                                     LeftHandSideExpression
                        );

  lazy val AdditiveOperator = "+" |
                         "-"  
                       ;

  lazy val MemberExpressionPart:Parser[AST.Expression=>AST.MemberAccessExpression] = "[" ~> Expression <~ "]" ^^ { x => { y:AST.Expression => AST.ArrayAccessExpression(y, x) } } |
                             "." ~> Identifier ^^ { x => { y:AST.Expression => AST.ObjectAccessExpression(y, x) } }  
                           ;

  lazy val BitwiseANDExpression = positioned(EqualityExpression * (BitwiseANDOperator ^^ makeBinaryOp)
                           );

  lazy val EqualityExpression = positioned(RelationalExpression * (EqualityOperator ^^ makeBinaryOp)
                         );

  lazy val VariableDeclarationList = rep1sep(VariableDeclaration, ",")
                              ;

  lazy val MultiplicativeExpression = positioned(UnaryExpression * (MultiplicativeOperator ^^ makeBinaryOp)
                               );

  lazy val ConditionalExpressionNoIn = positioned((LogicalORExpressionNoIn <~ "?") ~ (AssignmentExpression <~ ":") ~ AssignmentExpressionNoIn ^^ { case c ~ i ~ e => AST.ConditionalExpression(c, i, e) } |
                                             LogicalORExpressionNoIn
                                );

  lazy val BreakStatement = positioned("break" ~> opt(Identifier) <~ opt(";") ^^ { AST.BreakStatement(_) }
                     );

  lazy val VariableDeclarationListNoIn = rep1sep(VariableDeclarationNoIn, ",")
                                  ;

  def combineMember(soFar: AST.Expression, constructor: AST.Expression => AST.Expression) = {
    constructor(soFar)
  }
  
  lazy val MemberExpressionForIn = positioned(FunctionExpression |
                              PrimaryExpression ~ rep(MemberExpressionPart) ^^ { case start ~ mods => mods.foldLeft(start)(combineMember) }
                            );

  lazy val AssignmentExpression:Parser[AST.Expression] = positioned(LeftHandSideExpression ~ AssignmentOperator ~ AssignmentExpression ^^ { case lhs ~ op ~ rhs => AST.AssignmentExpression(lhs, op, rhs) } |
                             ConditionalExpression  
                           );

  lazy val SourceElements: Parser[List[AST.SourceElement]] = rep1(SourceElement) ^^ { _.map(x => {
    x match {
      case AST.ExpressionStatement(AST.ExpressionList(el)) => el.map(AST.ExpressionStatement(_))
      case _ => List(x)
    }
  }).reduceLeft((a ,b) => a ::: b) }
                     ;

  lazy val EqualityOperator = "==" |
                         "!=" |
                         "===" |
                         "!=="  
                       ;

  lazy val MultiplicativeOperator = "*" |
                               "/" |
                               "%"  
                             ;

  lazy val LogicalORExpressionNoIn = positioned(LogicalANDExpressionNoIn * (LogicalOROperator ^^ makeBinaryOp)
                              );

  lazy val ImportStatement = positioned("import" ~> Name ~ (opt("." ~> "*") <~ ";") ^^ { case names ~ wild => AST.ImportStatement(names, wild.isDefined) }
                      );

  lazy val IdentifierName = not(Keyword) ~> """[A-Za-z\$_][A-Za-z0-9\$_]*""".r;
  
  lazy val Identifier = positioned(IdentifierName ^^ { AST.Identifier(_) }
                 );

  lazy val Block: Parser[AST.Block] = positioned("{" ~> opt(StatementList) <~ "}" ^^ { AST.Block(_) }
            );

  lazy val MemberExpression = positioned((FunctionExpression | PrimaryExpression) ~ rep(MemberExpressionPart) ^^ { case start ~ mods => mods.foldLeft(start)(combineMember) } |
                         AllocationExpression
                       );

  lazy val ThrowStatement = positioned("throw" ~> Expression <~ opt(";") ^^ { AST.ThrowStatement(_) }
                     );

  lazy val RelationalExpression = positioned(ShiftExpression * (RelationalOperator ^^ makeBinaryOp)
                           );

  lazy val InitialiserNoIn = positioned("=" ~> AssignmentExpressionNoIn
                      );

  lazy val VariableStatement = positioned("var" ~> VariableDeclarationList <~ opt(";") ^^ { AST.VariableStatement(_) }
                        );

  lazy val BitwiseXOROperator = "^"
                         ;

  lazy val CallExpressionForIn = positioned(MemberExpressionForIn ~ Arguments ~ rep(CallExpressionPart) ^^ { case exp ~ args ~ parts => { parts.foldLeft(AST.CallExpression(exp, args).asInstanceOf[AST.Expression])(combineMember) } }
                          );

  lazy val CallExpression = positioned(MemberExpression ~ Arguments ~ rep(CallExpressionPart) ^^ { case exp ~ args ~ parts => { parts.foldLeft(AST.CallExpression(exp, args).asInstanceOf[AST.Expression])(combineMember) } }
                     );
  
  lazy val Literal:Parser[AST.Literal] = positioned(DecimalLiteral |
                HexIntegerLiteral |
                StringLiteral |
                BooleanLiteral |
                NullLiteral //|
                //RegularExpressionLiteral  
              );
  
  lazy val HexIntegerLiteral = """0[xX][0-9A-Fa-f]+""".r ^^ { AST.HexIntegerLiteral(_) };
  
  lazy val BooleanLiteral = "true" ^^^ { AST.BooleanLiteral(true) } | "false" ^^^ { AST.BooleanLiteral(false) }
  
  lazy val NullLiteral = "null" ^^^ { AST.NullLiteral() };

  lazy val Program = opt(SourceElements) ^^ { AST.Program(_) }
              ;

  lazy val VariableDeclaration = positioned(Identifier ~ opt(Initialiser) ^^ { case id ~ i => AST.VariableDeclaration(id, i) }
                          );

  lazy val ContinueStatement = positioned("continue" ~> opt(Identifier) <~ opt(";") ^^ { AST.ContinueStatement(_) }
                        );

  lazy val SwitchStatement = positioned(("switch" ~> "(" ~> Expression <~ ")") ~ CaseBlock ^^ { case e ~ cb => AST.SwitchStatement(e, cb) }
                      );

  lazy val BitwiseXORExpressionNoIn = positioned(BitwiseANDExpressionNoIn * (BitwiseXOROperator ^^ makeBinaryOp)
                               );

  lazy val RelationalExpressionNoIn = positioned(ShiftExpression * (RelationalNoInOperator ^^ makeBinaryOp)
                               );

  lazy val LogicalANDOperator = "&&"
                         ;

  //val JScriptVarDeclarationList = positioned(rep1sep(JScriptVarDeclaration, ",")
  //                              );

  lazy val PropertyName:Parser[AST.Expression] = positioned(Identifier |
                     StringLiteral |
                     DecimalLiteral  
                   );

  lazy val StringLiteral = """\"[^\"]*\"""".r ^^ { AST.StringLiteral(_) }
                    ;

  lazy val DecimalIntegerLiteral = "0" | """[1-9][0-9]*""".r 
                            ;

  lazy val DecimalLiteral = DecimalIntegerLiteral ~ "." ~ """[0-9]*""".r ~ """([Ee][+-]?[0-9]+)?""".r ^^ { case a~b~c~d => AST.DecimalLiteral(a+b+c+d) } |
                            "." ~ """[0-9]+""".r ~ """([Ee][+-]?[0-9]+)?""".r  ^^ { case a~b~c => AST.DecimalLiteral(a+b+c) } |
                            DecimalIntegerLiteral ~ """([Ee][+-]?[0-9]+)?""".r ^^ { case a~b => AST.DecimalLiteral(a+b) }
                     ;
  
  lazy val ArgumentMemberExpressionParts = Arguments ~ rep(MemberExpressionPart) ^^ { case args ~ parts => { exp:AST.Expression => { parts.foldLeft(AST.CallExpression(exp, args).asInstanceOf[AST.Expression])(combineMember) } } }
                                    ;
                     
  lazy val AllocationExpression:Parser[AST.AllocationExpression] = positioned(("new" ~> MemberExpression) ~ rep(ArgumentMemberExpressionParts) ^^ { case start ~ parts => { AST.AllocationExpression(parts.foldLeft(start)(combineMember)) } }
                           );

  lazy val Catch = positioned(("catch" ~> "(" ~> Identifier) ~ (")" ~> Block) ^^ { case id ~ block => AST.Catch(id, block) }
            );

  lazy val TryStatement = positioned("try" ~> Block ~ Finally ^^ { case b ~ f => AST.TryStatement(b, None, Some(f)) } |
                                "try" ~> Block ~ Catch ~ opt(Finally) ^^ { case b ~ c ~ f => AST.TryStatement(b, Some(c), f) }
                   );

  lazy val FormalParameterList = rep1sep(Identifier, ",")
                          ;

  lazy val BitwiseORExpression = positioned(BitwiseXORExpression * (BitwiseOROperator ^^ makeBinaryOp)
                          );

  lazy val Expression = positioned(rep1sep(AssignmentExpression, ",") ^^ { makeExpressionList(_) }
                 );

  lazy val AdditiveExpression = positioned(MultiplicativeExpression * (AdditiveOperator ^^ makeBinaryOp)
                         );

  lazy val ConditionalExpression = positioned((LogicalORExpression ~ ("?" ~> AssignmentExpression) ~ (":" ~> AssignmentExpression)) ^^ { case c ~ i ~ e => AST.ConditionalExpression(c, i, e) } |
                              LogicalORExpression
                            );
  
  lazy val UnaryExpression: Parser[AST.Expression] = positioned(PostfixExpression |
                        rep1(UnaryOperator) ~ PostfixExpression ^^ { case ops ~ e => { ops.foldRight(e)((op:String, soFar:AST.Expression) => { AST.UnaryExpression(op, soFar) }) } } 
                      );

  lazy val LeftHandSideExpression: Parser[AST.Expression] = positioned(CallExpression |
                               MemberExpression  
                             );

  lazy val FunctionDeclaration = positioned("function" ~> Identifier ~ ("(" ~> opt(FormalParameterList) <~ ")") ~ FunctionBody ^^ { 
                              case name ~ params ~ body => { AST.FunctionExpression(Some(name), params, body) }
                            }
                          );

  lazy val Initialiser = positioned("=" ~> AssignmentExpression
                  );

  lazy val CallExpressionPart:Parser[AST.Expression => AST.Expression] = Arguments ^^ { x => { y:AST.Expression => AST.CallExpression(y, x) } } |
                           "[" ~> Expression <~ "]" ^^ { x => { y:AST.Expression => AST.ArrayAccessExpression(y, x) } } |
                           "." ~> Identifier ^^ { x => { y:AST.Expression => AST.ObjectAccessExpression(y, x) } }
                         ;

  lazy val RelationalNoInOperator = "<" |
                               ">" |
                               "<=" |
                               ">=" |
                               "instanceof"  
                             ;

  lazy val AssignmentExpressionNoIn:Parser[AST.Expression] = positioned(LeftHandSideExpression ~ AssignmentOperator ~ AssignmentExpressionNoIn ^^ { case lhs ~ op ~ rhs => { AST.AssignmentExpression(lhs, op, rhs) } } |
                                 ConditionalExpressionNoIn  
                               );

  lazy val PrimaryExpression = positioned("this" ^^^ { AST.This() } |
                          ObjectLiteral |
                          "(" ~> Expression <~ ")" |
                          Identifier |
                          ArrayLiteral |
                          Literal
                        );

  lazy val LogicalANDExpressionNoIn = positioned(BitwiseORExpressionNoIn * (LogicalANDOperator ^^ makeBinaryOp)
                               );

  lazy val PropertyNameAndValueList = rep1sep(PropertyNameAndValue, ",") |
                                                           "," ^^^ { Nil }
                               ;

  lazy val Arguments = "(" ~> opt(ArgumentList) <~ ")"
                ;

  lazy val ObjectLiteral = positioned("{" ~> opt(PropertyNameAndValueList) <~ "}" ^^ { AST.ObjectLiteral(_) }
                    );

  lazy val ExpressionNoIn = rep1sep(AssignmentExpressionNoIn, ",") ^^ { makeExpressionList(_) }
                     ;

  lazy val LeftHandSideExpressionForIn = positioned(CallExpressionForIn |
                                    MemberExpressionForIn  
                                  );

  lazy val ElementList = opt(Elision) ~> rep1sep(AssignmentExpression, Elision)
                  ;

  lazy val LabelledStatement:Parser[AST.LabelledStatement] = positioned((Identifier <~ ":") ~ Statement ^^ { case i ~ s => AST.LabelledStatement(i, s) }
                        );

  lazy val DefaultClause = "default" ~> ":" ~> opt(StatementList) ^^ { AST.DefaultClause(_) }
                    ;

  lazy val BitwiseOROperator = "|"
                        ;

  lazy val UnaryOperator = "delete" |
                      "void" |
                      "typeof" |
                      "++" |
                      "--" |
                      "+" |
                      "-" |
                      "~" |
                      "!"  
                    ;

  lazy val RelationalOperator = "<" |
                           ">" |
                           "<=" |
                           ">=" |
                           "instanceof" |
                           "in"  
                         ;

  lazy val ShiftExpression = positioned(AdditiveExpression * (ShiftOperator ^^ makeBinaryOp)
                      );

  lazy val Name = rep1sep(IdentifierName, ".")
           ;

  lazy val BitwiseANDOperator = "&"
                         ;

  lazy val ArrayLiteral = positioned("[" ~> opt(Elision) <~ "]" ^^ { AST.ArrayLiteral(_) } |
                           "[" ~> ElementList ~ Elision <~ "]" ^^ { case a ~ b => AST.ArrayLiteral(Some(a ++ b)) } |
                           "[" ~> opt(ElementList) <~ "]" ^^ { AST.ArrayLiteral(_) }  
                   );

  lazy val EqualityExpressionNoIn = positioned(RelationalExpressionNoIn * (EqualityOperator ^^ makeBinaryOp)
                             );

  lazy val ShiftOperator = "<<" |
                      ">>" |
                      ">>>"  
                    ;

  lazy val PropertyNameAndValue = (PropertyName <~ ":") ~ AssignmentExpression ^^ { case a ~ b => AST.KVPair(a, b) }
                           ;

  lazy val LogicalORExpression = positioned(LogicalANDExpression * (LogicalOROperator ^^ makeBinaryOp)
                          );

  lazy val BitwiseXORExpression = positioned(BitwiseANDExpression * (BitwiseXOROperator ^^ makeBinaryOp)
                           );

  //val JScriptVarStatement = positioned("var" ~> JScriptVarDeclarationList <~ opt(";") ^^ { AST.VariableStatement(_) }
  //                        );

  lazy val WithStatement:Parser[AST.WithStatement] = positioned(("with" ~> "(" ~> Expression <~ ")") ~ Statement ^^ { case e ~ s => { AST.WithStatement(e, s) } }
                    );

  //val JScriptVarDeclaration = positioned(Identifier ~ ":" ~ IdentifierName ~ opt(Initialiser)
  //                          );

  lazy val IterationStatement:Parser[AST.Statement] = positioned(("do" ~> Statement) ~ (("while" ~> "(") ~> Expression <~ (")" <~ opt(";"))) ^^ { case s ~ e => AST.DoWhileStatement(e ,s) } |
                           "while" ~> "(" ~> Expression ~ (")" ~> Statement) ^^ { case e ~ s => AST.WhileStatement(e, s) } |
                           (("for" ~> "(" ~> opt(ExpressionNoIn)) ~ (";" ~> opt(Expression) <~ ";") ~ (opt(Expression) <~ ")") ~ Statement) ^^
                           { case e1 ~ e2 ~ e3 ~ s => { AST.ForStatement(e1, e2, e3, s) } } |
                           (("for" ~> "(" ~> "var" ~> VariableDeclarationList) ~ (";" ~> opt(Expression)) ~ (";" ~> opt(Expression) <~ ")") ~ Statement) ^^
                           { case decl ~ e2 ~ e3 ~ s => { AST.ForStatement(Some(AST.VariableStatement(decl)), e2, e3, s) } } |
                           (("for" ~> "(" ~> "var" ~> VariableDeclarationNoIn) ~ ("in" ~> Expression <~ ")") ~ Statement) ^^
                           { case decl ~ e ~ s =>  { AST.ForInStatement(decl, e, s) } } |
                           (("for" ~> "(" ~> LeftHandSideExpressionForIn) ~ ("in" ~> Expression <~ ")") ~ Statement) ^^
                           { case lhs ~ e ~ s => { AST.ForInStatement(lhs, e, s) } }
                         );

  lazy val IfStatement:Parser[AST.IfStatement] = positioned(("if" ~> "(" ~> Expression <~ ")") ~ Statement ~ opt("else" ~> Statement) ^^ { case cond ~ i ~ e => { AST.IfStatement(cond, i, e) } }
                  );
  
  def makeBinaryOp(op: String) =
    (fact1: AST.Expression, fact2: AST.Expression) => AST.BinaryExpression(op, fact1, fact2)
  
  def makeExpressionList(el: List[AST.Expression]) = {
    if (el.length > 1) {
      AST.ExpressionList(el)
    } else {
      el(0)
    }
  }
    
  /* APPLY */
  def apply(input: String): AST.Program = parseAll(Program, input) match {
	  			case Success(result, _) => result
	  			case failure: NoSuccess => scala.sys.error(failure.msg + " @ line %d:%d".format(failure.next.pos.line, failure.next.pos.column) + "\n" + failure.next.pos.longString)
	  		}
}
