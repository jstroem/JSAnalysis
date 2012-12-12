package JSAnalyzer

import scala.collection.mutable.HashMap

object SSA {
  type DUChain = Map[CFG.ControlFlowNode, Map[AST.Identifier, Set[CFG.ControlFlowNode]]]
  type Dom = Map[CFG.ControlFlowNode, Set[CFG.ControlFlowNode]]
  type VarDef = Map[AST.Identifier, Set[CFG.ControlFlowNode]]
  
  def apply(bcfg : CFG.ControlFlowGraph, defuse : DUChain, domTree : CFG.ControlFlowGraph, df : Dom) = {
    def varsCombine(m1 : VarDef, m2 : VarDef) : VarDef = {
      var newM1 = m1
      var newM2 = m2
      val keys = m1.keys.toSet.union(m2.keys.toSet)
      keys.foreach {
        k => {
          if (!m1.contains(k)) newM1 = newM1 + (k -> Set[CFG.ControlFlowNode]())
          if (!m2.contains(k)) newM2 = newM2 + (k -> Set[CFG.ControlFlowNode]())
        }
      }
      
      newM1.map { case (key, value) => (key, value.union(newM2(key))) } toMap
    }
    
    val rbcfg = (bcfg.nodes.map {
      case node : CFG.Block => node.lst.map {
        n => (n, node : CFG.ControlFlowNode)
      }
    }).flatten.toMap

    val vars = defuse.values.foldLeft(Map[AST.Identifier, Set[CFG.ControlFlowNode]]()) { (soFar, e) => varsCombine(soFar, e) }
    
    var phiNodes = Map[CFG.ControlFlowNode, Set[AST.Identifier]]()

    // Figure out where to insert phi nodes
    
    vars.foreach {
      case (v, defs) => {
        var worklist = defs.map(rbcfg(_))
        var visited = Set[CFG.ControlFlowNode]()
        while (worklist.size != 0) {
          val b = worklist.head
          worklist = worklist - b
          if (df.contains(b)) {
            df(b).foreach {
              d => phiNodes = phiNodes + (d -> (phiNodes.getOrElse(d, Set[AST.Identifier]()) + v))
              if (!visited.contains(d)) worklist = worklist + d
            }
          }
          visited = visited + b
        }
      }
    }
    
    val aPhiNodes = phiNodes.map({ case (node, set) => (node, set.map(id => CFG.Phi(AST.Identifier(id.value)))) }).toMap
    
    // Preorder traversal of the dominator tree
    val head = bcfg.start
    
    var stacks = vars.keys.map({ k => (k.value, 0) }).toMap
    
    var changed = Set[String]()

    def newName(id: AST.Identifier) = {
      val orig = id.value.split("%").head
      
      //println("Newname(" + orig + ")")
      
      if (stacks.contains(orig)) {
        stacks = stacks + (orig -> (stacks(orig) + 1))
        changed = changed + orig
        renameVar(id)
      }
      
      //println(" = " + id.value.toString())
    }
    
    def currentVersion(id : AST.Identifier) = {
      val orig = id.value.split("%").head
      
      if (!stacks.contains(orig)) {
        "__illegal_var__"
      } else {
        orig + "%" + stacks(orig).toString()
      }
    }
    
    def renameVar(id : AST.Identifier) = {
      val orig = id.value.split("%").head
      
      if (stacks.contains(orig)) {
        id.value = currentVersion(id)
      }
    }
    
    def renameVars(node : CFG.ControlFlowNode) = {
      def expressionType(exp : AST.Expression) : Unit = {
        exp match {
          case AST.ExpressionList(es) => es.map(expressionType(_))
          case AST.AssignmentExpression(e1, op, e2) => e1 match {
            case i : AST.Identifier => expressionType(e2); newName(i)
            case AST.ArrayAccessExpression(i : AST.Identifier, e) => expressionType(e2); expressionType(e); newName(i)
            case AST.ObjectAccessExpression(i : AST.Identifier, i2) => expressionType(e2); newName(i)
            case _ => expressionType(e2); expressionType(e1)
          }
          case AST.BinaryExpression(op, e1, e2) => expressionType(e2); expressionType(e1)
          case AST.UnaryExpression(op, e) => expressionType(e)
          case AST.PostfixExpression(op, e) => expressionType(e)
          case AST.ConditionalExpression(e1, e2, e3) => expressionType(e3); expressionType(e2); expressionType(e1)
          case AST.CallExpression(e, oes) => expressionType(e);
          oes match {
            case Some(es) => es.map(expressionType(_))
            case None =>
          }
          case AST.AllocationExpression(e) => expressionType(e)
          case AST.ArrayAccessExpression(e1, e2) => expressionType(e2); expressionType(e1)
          case AST.ObjectAccessExpression(e, i) => expressionType(e)
          case AST.ArrayLiteral(oes) => oes match {
            case Some(es) => es.map(expressionType(_))
            case None =>
          }
          case id : AST.Identifier => renameVar(id)
          case _ =>
        }
      }

      node match {
        case CFG.Continue(oe, _) => oe foreach {
          expressionType(_)
        }
        case CFG.Return(oe, _) => oe foreach {
          expressionType(_)
        }
        case CFG.Expression(e, _) => expressionType(e)
        case CFG.Assignment(i, e, _) => e foreach {
          e => expressionType(e); newName(i)
        }
        case CFG.If(e, _) => expressionType(e)
        case CFG.DoWhile(e, _) => expressionType(e)
        case CFG.ForIn(e1, e2, _) => expressionType(e2)
        case CFG.With(e, _) => expressionType(e)
        case CFG.Switch(e, _) => expressionType(e)
        case CFG.CaseClause(e, _) => expressionType(e)
        case _ => 
      }
    }
    
    def visit(block : CFG.Block) : Unit = {
      val backupStacks = stacks
      changed = Set[String]()
      
      //println("%" + CFGGrapher.nodeToString(block) + "%")
      
      val head = block.lst.head match {
        case n : CFG.Merge => List(n)
        case _ => List[CFG.ControlFlowNode]()
      }
      val tail = block.lst.head match {
        case n : CFG.Merge => block.lst.tail
        case _ => block.lst
      }
      if (aPhiNodes.contains(block)) {
        aPhiNodes(block).foreach {
          phi => newName(phi.varName)
        }
        
        block.lst = head ++ aPhiNodes(block) ++ tail
      }
      
      tail.foreach {
        renameVars(_)
      }
      
      bcfg.edges.filter({ case (from, to) => from == block }).map(_._2).foreach {
        case b : CFG.Block => {
          if (aPhiNodes.contains(b)) {
            aPhiNodes(b).foreach {
              phi => 
              val orig = phi.varName.value.split("%").head
              //println(phi.options.map(_.value).mkString(", "))
            }
          }
        }
      }
      
      domTree.edges.filter({ case (from, to) => from == block }).map(_._2).foreach {
        case b : CFG.Block => visit(b)
      }
      
      //stacks = backupStacks
    }
    
    head match {
      case b : CFG.Block => visit(b)
    }
    
    /*bcfg.nodes.foreach {
      case node : CFG.Block => {
        val head = node.lst.head match {
          case h : CFG.Merge => List(h)
          case _ => List[CFG.ControlFlowNode]()
        }
        val tail = node.lst.head match {
          case h : CFG.Merge => node.lst.tail
          case _ => node.lst
        }
        node.lst = (head ++ phiNodes.getOrElse(node, Set[AST.Identifier]()).map { CFG.Phi(_) }).toList ++ tail
      }
    }*/
    
    bcfg
  }
}