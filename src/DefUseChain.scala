package JSAnalyzer

object DefUseChain {
  
  type Domain = Map[AST.Identifier, Set[CFG.ControlFlowNode]]
  type Edge = (CFG.ControlFlowNode, CFG.ControlFlowNode)
  
  class ReachingDefs(cfg : CFG.ControlFlowGraph) extends DataFlowAnalysis.DataFlowAnalysis[Domain] {
    lazy val bottom = getRDLatticeBottom()
    lazy val top = getRDLatticeTop()
    
    lazy val assignedVars = cfg.nodes.map(n => (n, getAssignedVars(n))).toMap
    lazy val vars = assignedVars.values.foldLeft(Set[AST.Identifier]())({ (soFar, e) => soFar.union(e) })
    
    def getBottom = {
      bottom
    }

    def getTop = {
      top
    }
    
    def compareElements(m1 : Domain, m2 : Domain) : Option[Boolean] = {
      if (m1.foldLeft(true) { case (bool, (key, value)) => bool && value.intersect(m2(key)) == value }) {
        Some(true)
      } else if (m2.foldLeft(true) { case (bool, (key, value)) => bool && value.intersect(m1(key)) == value }) {
        Some(false)
      } else {
        None
      }
    }

    def getLub(m1 : Domain, m2 : Domain) : Domain = {
      m1.map { case (key, value) => (key, value.union(m2(key))) } toMap
    }
    
    def getGlb(m1 : Domain, m2 : Domain) : Domain = {
      m1.map { case (key, value) => (key, value.intersect(m2(key))) } toMap
    }
    
    def printInfo(info: Domain) = {
      info.map({
        case (v, dn) => {
          v.value + " -> [ " + dn.map({ CFGGrapher.nodeToString(_) }).mkString(", ") + " ]"
        }
      }).mkString(", ")
    }
    
    def globalFlowFunction(node : CFG.ControlFlowNode, info_in1 : List[Domain], 
        lattice : DataFlowAnalysis.DataFlowAnalysis[Domain]) = {		
      val info_in = if (info_in1.isEmpty) lattice.getBottom::info_in1 else info_in1
      
      node match {
        case CFG.Merge(_,_) => info_in.foldLeft(lattice.getBottom)((map, e) => lattice.getLub(map, e))
        case _ => {
          val vars = assignedVars(node)
          val changedVars = info_in.head.filter({ case (key, value) => vars.contains(key) }).keys
          val killed = (info_in.head -- changedVars)
          val added = (vars.map { (_, Set(node)) } toMap)
          val res = killed ++ added
          res
        }
      }
    }

    def getAssignedVars (node: CFG.ControlFlowNode) : Set[AST.Identifier] = {
      def expressionType(exp : AST.Expression, vars : Set[AST.Identifier]) : Set[AST.Identifier] = {
        exp match {
          case AST.ExpressionList(es) => es.foldLeft(vars)((list, exp) => expressionType(exp, list))
          case AST.AssignmentExpression(e1, op, e2) => e1 match {
            case e : AST.Identifier => vars + e
            case AST.ArrayAccessExpression(i : AST.Identifier, e2) => expressionType(e2, vars)
            case AST.ObjectAccessExpression(i : AST.Identifier, i2) => vars + i
            case _ => vars
          }
          case AST.BinaryExpression(op, e1, e2) => expressionType(e2, expressionType(e1, vars))
          case AST.UnaryExpression(op, e) => expressionType(e, vars)
          case AST.PostfixExpression(op, e) => expressionType(e, vars)
          case AST.ConditionalExpression(e1, e2, e3) => expressionType(e3, expressionType(e2, expressionType(e1, vars)))
          case AST.CallExpression(e, oes) => expressionType(e, oes match {
            case Some(es) => es.foldLeft(vars)((list, exp) => expressionType(exp, list))
            case None => vars
          })
          case AST.AllocationExpression(e) => expressionType(e, vars)
          case AST.ArrayAccessExpression(e1, e2) => expressionType(e2, expressionType(e1, vars))
          case AST.ObjectAccessExpression(e, i) => expressionType(e, vars)
          case AST.ArrayLiteral(oes) => oes match {
            case Some(es) => es.foldLeft(vars)((list, exp) => expressionType(exp, list))
            case None => vars
          }
          case _ => vars
        }
      }

      val list = Set[AST.Identifier]()
      
      (node match {
        case CFG.Continue(oe, _) => oe match {
          case Some(e) => expressionType(e, list)
          case None => list
        }
        case CFG.Return(oe, _) => oe match {
          case Some(e) => expressionType(e, list)
          case None => list
        }
        case CFG.Expression(e, _) => expressionType(e, list)
        case CFG.Assignment(i, e, _) => e match {
          case Some(e) => expressionType(e, list) + i
          case None => list
        }
        case CFG.If(e, _) => expressionType(e, list)
        case CFG.DoWhile(e, _) => expressionType(e, list)
        case CFG.ForIn(e1, e2, _) => e1 match {
          case e1: AST.Identifier => expressionType(e2, list) + e1
          case _ => expressionType(e2, list)
        }
        case CFG.With(e, _) => expressionType(e, list)
        case CFG.Switch(e, _) => expressionType(e, list)
        case CFG.CaseClause(e, _) => expressionType(e, list)
        
        case CFG.Block(lst, _) => lst.map(getAssignedVars).foldLeft(Set[AST.Identifier]()){ (soFar, e) => soFar.union(e) }
        case _ => list;
      }) 
    }
    
    def useDefChain(cfg : CFG.ControlFlowGraph, reachingDefs : Map[Edge, Domain]) = {
      val uses = getAllUsedVars(cfg)
      cfg.nodes map { node =>
        (node, 
        (((reachingDefs filter { 
          case ((from, to), myDefs) => to == node 
        }).values.foldLeft(getBottom) {
          (map : Domain, e : Domain) => getLub(map, e)
        }).filter {
          case (id, dn) => uses(node).contains(id)
        }))
      } toMap
    }

    def getRDLatticeBottom() : Domain = {
      vars map { (_, Set[CFG.ControlFlowNode]()) } toMap
    }

    def getRDLatticeTop() : Domain = {
      vars map { (_, cfg.nodes toSet) } toMap
    }
  }
  
  def printChain(chain : Map[CFG.ControlFlowNode, Domain]) = {
    chain.map({
      case (node, defs) => "%" + CFGGrapher.nodeToString(node) + "% : " + defs.map({ 
        case (v, dn) => v.value + " -> " + Dominance.printWorklist(dn toList) }).mkString(", ")
    }).mkString("\n")
  }
  
  def printAssignedVars(vars : Map[CFG.ControlFlowNode, List[AST.Identifier]]) = {
    vars.map({
      case (node, vars) => CFGGrapher.nodeToString(node) + " : " + vars.map(_.value).mkString(", ")
    }).mkString("\n")
  }
  
  def printReachingDefs(rDefs : Map[Edge, Domain]) = {
    rDefs.map({
      case ((from, to), defs) => "(" + CFGGrapher.nodeToString(from) + " -> " + CFGGrapher.nodeToString(to) + ") : " + defs.map({
        case (v, dn) => v.value + " -> " + Dominance.printWorklist(dn toList) 
      }).mkString(", ")
    }).mkString("\n")
  }

  def makeGraph(cfg: CFG.ControlFlowGraph, chain: Map[CFG.ControlFlowNode, Domain]): CFG.ControlFlowGraph = {
    val edgeLabels = (chain map {
      case (node, defs) => defs map {
        case (id, dn) => dn map {
          n => ((node, n), id.value)
        }
      } flatten
    } flatten)
    val edges = cfg.edges ++ edgeLabels.map(_._1)
    val labels = cfg.labels ++ (edgeLabels toMap)

    CFG.ControlFlowGraph(cfg.start, cfg.end, cfg.nodes, edges, labels, cfg.info)
  }

  def getUsedVars(node: CFG.ControlFlowNode): Set[AST.Identifier] = {
    def expressionType(exp: AST.Expression, vars: Set[AST.Identifier]): Set[AST.Identifier] = {
      exp match {
        case AST.ExpressionList(es) => es.foldLeft(vars)((list, exp) => expressionType(exp, list))
        case AST.AssignmentExpression(e1, op, e2) => e1 match {
          case i: AST.Identifier => if (op != "=") expressionType(e2, vars) + i else expressionType(e2, vars)
          case AST.ArrayAccessExpression(i: AST.Identifier, e) => if (op != "=") expressionType(e2, expressionType(e, vars)) + i else expressionType(e2, expressionType(e, vars))
          case AST.ObjectAccessExpression(i: AST.Identifier, i2) => if (op != "=") expressionType(e2, vars) + i else expressionType(e2, vars)
          case _ => expressionType(e2, expressionType(e1, vars))
        }
        case AST.BinaryExpression(op, e1, e2) => expressionType(e2, expressionType(e1, vars))
        case AST.UnaryExpression(op, e) => expressionType(e, vars)
        case AST.PostfixExpression(op, e) => expressionType(e, vars)
        case AST.ConditionalExpression(e1, e2, e3) => expressionType(e3, expressionType(e2, expressionType(e1, vars)))
        case AST.CallExpression(e, oes) => expressionType(e, oes match {
          case Some(es) => es.foldLeft(vars)((list, exp) => expressionType(exp, list))
          case None => vars
        })
        case AST.AllocationExpression(e) => expressionType(e, vars)
        case AST.ArrayAccessExpression(e1, e2) => expressionType(e2, expressionType(e1, vars))
        case AST.ObjectAccessExpression(e, i) => expressionType(e, vars)
        case AST.ArrayLiteral(oes) => oes match {
          case Some(es) => es.foldLeft(vars)((list, exp) => expressionType(exp, list))
          case None => vars
        }
        case id: AST.Identifier => vars + id
        case _ => vars
      }
    }

    val list = Set[AST.Identifier]()

    node match {
      case CFG.Continue(oe, _) => oe match {
        case Some(e) => expressionType(e, list)
        case None => list
      }
      case CFG.Return(oe, _) => oe match {
        case Some(e) => expressionType(e, list)
        case None => list
      }
      case CFG.Expression(e, _) => expressionType(e, list)
      case CFG.Assignment(i, e, _) => e match {
        case Some(e) => expressionType(e, list)
        case None => list
      }
      case CFG.If(e, _) => expressionType(e, list)
      case CFG.DoWhile(e, _) => expressionType(e, list)
      case CFG.ForIn(e1, e2, _) => expressionType(e2, list)
      case CFG.With(e, _) => expressionType(e, list)
      case CFG.Switch(e, _) => expressionType(e, list)
      case CFG.CaseClause(e, _) => expressionType(e, list)

      case CFG.Block(lst, _) => lst.map(getUsedVars).foldLeft(Set[AST.Identifier]()) { (soFar, e) => soFar.union(e) }
      case _ => list;
    }
  }

  def getAllUsedVars(cfg: CFG.ControlFlowGraph) = {
    (cfg.nodes.map { node => (node, getUsedVars(node)) }) toMap
  }
}