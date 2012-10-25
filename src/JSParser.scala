import scala.util.parsing.combinator.RegexParsers

class JSParser extends RegexParsers {
  
  val Keyword = "break" |
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
  
  val FunctionBody = "{" ~ opt(SourceElements) ~ "}"
                   ;

  val BitwiseANDExpressionNoIn = EqualityExpressionNoIn ~ rep(BitwiseANDOperator ~ EqualityExpressionNoIn)
                               ;

  val Elision = rep1(",")
              ;

  val Statement = Block |
                  JScriptVarStatement |
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
                ;

  val VariableDeclarationNoIn = Identifier ~ opt(InitialiserNoIn)
                              ;

  val LogicalANDExpression = BitwiseORExpression ~ rep(LogicalANDOperator ~ BitwiseORExpression)
                           ;

  val ArgumentList = rep1sep(AssignmentExpression, ",")
                   ;

  val LogicalOROperator = "||"
                        ;

  val PostfixOperator = "++" | "--"
                      ;

  val ExpressionStatement = Expression ~ opt(";")
                          ;

  val CaseClauses = rep1(CaseClause)
                  ;

  val StatementList = rep1(Statement)
                    ;

  val BitwiseORExpressionNoIn = BitwiseXORExpressionNoIn ~ rep(BitwiseOROperator ~ BitwiseXORExpressionNoIn)
                              ;

  val CaseBlock = "{" ~ opt(CaseClauses) ~ "}" |
                  DefaultClause ~ opt(CaseClauses) ~ "}"  
                ;

  val AssignmentOperator = "=" |
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

  val FunctionExpression = "function" ~ opt(Identifier) ~ "(" ~ opt(FormalParameterList) ~ ")" ~ FunctionBody
                         ;

  val Finally = "finally" ~ Block
              ;

  val SourceElement = FunctionDeclaration |
                      Statement  
                    ;

  val CaseClause = "case" ~ Expression ~ ":" ~ opt(StatementList)
                 ;

  val EmptyStatement = ";"
                     ;

  val ReturnStatement = "return" ~ opt(Expression) ~ opt(";")
                      ;

  val PostfixExpression = LeftHandSideExpression ~ opt(PostfixOperator)
                        ;

  val AdditiveOperator = "+" |
                         "-"  
                       ;

  val MemberExpressionPart = "[" ~ Expression ~ "]" |
                             "." ~ Identifier  
                           ;

  val BitwiseANDExpression = EqualityExpression ~ rep(BitwiseANDOperator ~ EqualityExpression)
                           ;

  val EqualityExpression = RelationalExpression ~ rep(EqualityOperator ~ RelationalExpression)
                         ;

  val VariableDeclarationList = VariableDeclaration ~ rep("," ~ VariableDeclaration)
                              ;

  val MultiplicativeExpression = UnaryExpression ~ rep(MultiplicativeOperator ~ UnaryExpression)
                               ;

  val ConditionalExpressionNoIn = LogicalORExpressionNoIn ~ opt("?" ~ AssignmentExpression ~ ":" ~ AssignmentExpressionNoIn)
                                ;

  val BreakStatement = "break" ~ opt(Identifier) ~ opt(";")
                     ;

  val VariableDeclarationListNoIn = VariableDeclarationNoIn ~ rep("," ~ VariableDeclarationNoIn)
                                  ;

  val MemberExpressionForIn = FunctionExpression |
                              PrimaryExpression ~ rep(MemberExpressionPart)  
                            ;

  val AssignmentExpression = LeftHandSideExpression ~ AssignmentOperator ~ AssignmentExpression |
                             ConditionalExpression  
                           ;

  val SourceElements = rep1(SourceElement)
                     ;

  val EqualityOperator = "==" |
                         "!=" |
                         "===" |
                         "!=="  
                       ;

  val MultiplicativeOperator = "*" |
                               "/" |
                               "%"  
                             ;

  val LogicalORExpressionNoIn = LogicalANDExpressionNoIn ~ rep(LogicalOROperator ~ LogicalANDExpressionNoIn)
                              ;

  val ImportStatement = "import" ~ Name ~ opt("." ~ "*") ~ ";"
                      ;

  val IdentifierName = not(Keyword) ~ """[A-Za-z\$_][A-Za-z0-9\$_]+""".r
  
  val Identifier = IdentifierName
                 ;

  val Block = "{" ~ opt(StatementList) ~ "}"
            ;

  val MemberExpression = FunctionExpression |
                         PrimaryExpression ~ rep(MemberExpressionPart) |
                         AllocationExpression  
                       ;

  val ThrowStatement = "throw" ~ Expression ~ opt(";")
                     ;

  val RelationalExpression = ShiftExpression ~ rep(RelationalOperator ~ ShiftExpression)
                           ;

  val InitialiserNoIn = "=" ~ AssignmentExpressionNoIn
                      ;

  val VariableStatement = "var" ~ VariableDeclarationList ~ opt(";")
                        ;

  val BitwiseXOROperator = "^"
                         ;

  val CallExpressionForIn = MemberExpressionForIn ~ Arguments ~ rep(CallExpressionPart)
                          ;

  val CallExpression = MemberExpression ~ Arguments ~ rep(CallExpressionPart)
                     ;

  val Literal = DecimalLiteral |
                HexIntegerLiteral |
                StringLiteral |
                BooleanLiteral |
                NullLiteral //|
                //RegularExpressionLiteral  
              ;
  
  val HexIntegerLiteral = """0[xX][0-9A-Fa-f]+""".r
  
  val BooleanLiteral = "true" | "false"
  
  val NullLiteral = "null"

  val Program = opt(SourceElements)
              ;

  val VariableDeclaration = Identifier ~ opt(Initialiser)
                          ;

  val ContinueStatement = "continue" ~ opt(Identifier) ~ opt(";")
                        ;

  val SwitchStatement = "switch" ~ "(" ~ Expression ~ ")" ~ CaseBlock
                      ;

  val BitwiseXORExpressionNoIn = BitwiseANDExpressionNoIn ~ rep(BitwiseXOROperator ~ BitwiseANDExpressionNoIn)
                               ;

  val RelationalExpressionNoIn = ShiftExpression ~ rep(RelationalNoInOperator ~ ShiftExpression)

  val LogicalANDOperator = "&&"
                         ;

  val JScriptVarDeclarationList = rep1sep(JScriptVarDeclaration, ",")
                                ;

  val PropertyName = Identifier |
                     StringLiteral |
                     DecimalLiteral  
                   ;

  val StringLiteral = """\"[^\"]*\"""".r

  val DecimalIntegerLiteral = "0" | """[1-9][0-9]*""".r

  val DecimalLiteral = DecimalIntegerLiteral ~ "." ~ """[0-9]*""".r ~ """([Ee][+-]?[0-9]+)?""".r
  
  val AllocationExpression = "new" ~ MemberExpression ~ rep(Arguments ~ rep(MemberExpressionPart))
                           ;

  val Catch = "catch" ~ "(" ~ Identifier ~ ")" ~ Block
            ;

  val TryStatement = "try" ~ Block ~ Finally |
                                     Catch ~ opt(Finally)  
                   ;

  val FormalParameterList = Identifier ~ rep("," ~ Identifier)
                          ;

  val BitwiseORExpression = BitwiseXORExpression ~ rep(BitwiseOROperator ~ BitwiseXORExpression)
                          ;

  val Expression = AssignmentExpression ~ rep("," ~ AssignmentExpression)
                 ;

  val AdditiveExpression = MultiplicativeExpression ~ rep(AdditiveOperator ~ MultiplicativeExpression)
                         ;

  val ConditionalExpression = LogicalORExpression ~ opt("?" ~ AssignmentExpression ~ ":" ~ AssignmentExpression)
                            ;

  val UnaryExpression = PostfixExpression |
                        rep1(UnaryOperator ~ UnaryExpression)  
                      ;

  val LeftHandSideExpression = CallExpression |
                               MemberExpression  
                             ;

  val FunctionDeclaration = "function" ~ Identifier ~ "(" ~ opt(FormalParameterList) ~ ")" ~ FunctionBody
                          ;

  val Initialiser = "=" ~ AssignmentExpression
                  ;

  val CallExpressionPart = Arguments |
                           "[" ~ Expression ~ "]" |
                           "." ~ Identifier  
                         ;

  val RelationalNoInOperator = "<" |
                               ">" |
                               "<=" |
                               ">=" |
                               "instanceof"  
                             ;

  val AssignmentExpressionNoIn = LeftHandSideExpression ~ AssignmentOperator ~ AssignmentExpressionNoIn |
                                 ConditionalExpressionNoIn  
                               ;

  val PrimaryExpression = "this" |
                          ObjectLiteral |
                          "(" ~ Expression ~ ")" |
                          Identifier |
                          ArrayLiteral |
                          Literal  
                        ;

  val LogicalANDExpressionNoIn = BitwiseORExpressionNoIn ~ rep(LogicalANDOperator ~ BitwiseORExpressionNoIn)
                               ;

  val PropertyNameAndValueList = PropertyNameAndValue ~ rep("," ~ PropertyNameAndValue |
                                                           ",")  
                               ;

  val Arguments = "(" ~ opt(ArgumentList) ~ ")"
                ;

  val ObjectLiteral = "{" ~ opt(PropertyNameAndValueList) ~ "}"
                    ;

  val ExpressionNoIn = AssignmentExpressionNoIn ~ rep("," ~ AssignmentExpressionNoIn)
                     ;

  val LeftHandSideExpressionForIn = CallExpressionForIn |
                                    MemberExpressionForIn  
                                  ;

  val ElementList = opt(Elision) ~ AssignmentExpression ~ rep(Elision ~ AssignmentExpression)
                  ;

  val LabelledStatement = Identifier ~ ":" ~ Statement
                        ;

  val DefaultClause = "default" ~ ":" ~ opt(StatementList)
                    ;

  val BitwiseOROperator = "|"
                        ;

  val UnaryOperator = "delete" |
                      "void" |
                      "typeof" |
                      "++" |
                      "--" |
                      "+" |
                      "-" |
                      "~" |
                      "!"  
                    ;

  val RelationalOperator = "<" |
                           ">" |
                           "<=" |
                           ">=" |
                           "instanceof" |
                           "in"  
                         ;

  val ShiftExpression = AdditiveExpression ~ rep(ShiftOperator ~ AdditiveExpression)
                      ;

  val Name = IdentifierName ~ rep("." ~ IdentifierName)
           ;

  val BitwiseANDOperator = "&"
                         ;

  val ArrayLiteral = "[" ~ opt(Elision) ~ "]" |
                           ElementList ~ Elision ~ "]" |
                           opt(ElementList) ~ "]"  
                   ;

  val EqualityExpressionNoIn = RelationalExpressionNoIn ~ rep(EqualityOperator ~ RelationalExpressionNoIn)
                             ;

  val ShiftOperator = "<<" |
                      ">>" |
                      ">>>"  
                    ;

  val PropertyNameAndValue = PropertyName ~ ":" ~ AssignmentExpression
                           ;

  val LogicalORExpression = LogicalANDExpression ~ rep(LogicalOROperator ~ LogicalANDExpression)
                          ;

  val BitwiseXORExpression = BitwiseANDExpression ~ rep(BitwiseXOROperator ~ BitwiseANDExpression)
                           ;

  val JScriptVarStatement = "var" ~ JScriptVarDeclarationList ~ opt(";")
                          ;

  val WithStatement = "with" ~ "(" ~ Expression ~ ")" ~ Statement
                    ;

  val JScriptVarDeclaration = Identifier ~ ":" ~ IdentifierName ~ opt(Initialiser)
                            ;

  val IterationStatement = "do" ~ Statement ~ "while" ~ "(" ~ Expression ~ ")" ~ opt(";") |
                           "while" ~ "(" ~ Expression ~ ")" ~ Statement |
                           "for" ~ "(" ~ opt(ExpressionNoIn) ~ ";" ~ opt(Expression) ~ ";" ~ opt(Expression) ~ ")" ~ Statement |
                           "for" ~ "(" ~ "var" ~ VariableDeclarationList ~ ";" ~ opt(Expression) ~ ";" ~ opt(Expression) ~ ")" ~ Statement |
                           "for" ~ "(" ~ "var" ~ VariableDeclarationNoIn ~ "in" ~ Expression ~ ")" ~ Statement |
                           "for" ~ "(" ~ LeftHandSideExpressionForIn ~ "in" ~ Expression ~ ")" ~ Statement  
                         ;

  val IfStatement = "if" ~ "(" ~ Expression ~ ")" ~ Statement ~ opt("else" ~ Statement)
                  ;
}
