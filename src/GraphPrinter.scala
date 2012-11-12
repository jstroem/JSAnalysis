package JSAnalyzer

import scala.collection.mutable.HashMap

class GraphNode(name: String, attributes: List[(String, String)]) {
  var id: String = ""

  def makeLabel() = {
    var res: String = ""

    if (attributes.length > 0) {
      res = "{%s".format(name)
      for ((attr, value) <- attributes) {
        var newVal = value.stripPrefix("\"").stripSuffix("\"")
        newVal = newVal.map(_ match { 
          case '\'' => "&apos;"
          case '"' => "&quot;"
          case '<' => "&lt;"
          case '>' => "&gt;"
          case other => other toString
        }) mkString;
        res += "|{%s|%s}".format(attr, newVal)
      }
      res += "}"
    } else {
      res = name
    }

    res
  }

  def setId(counter: Int) {
    id = "n%s".format(counter)
  }
}

class GraphLink(val from: GraphNode, val to: GraphNode, val label: String)

class GraphPrinter(start: AST.ASTNode) {
  var nodes: List[GraphNode] = Nil
  var links: List[GraphLink] = Nil
  var ranks = HashMap.empty[Int, List[GraphNode]]

  var counter = 0

  def print(export: java.io.PrintStream = System.out) = {
    start.graphPrint(this, 0)

    export.println("digraph G {")
    for (node <- nodes) {
      export.println("\t%s [shape=record label=\"%s\"];".format(node.id, node.makeLabel()))
    }

    export.println("")

    for (link <- links) {
      export.println("\t%s -> %s [label=\"%s\"];".format(link.from.id, link.to.id, link.label))
    }

    export.println("")

    ranks.foreach {
      case (key, value) => export.println("\t{ rank=same; %s }".format(value.map(_.id).reduceLeft(_ + " " + _)))
    }

    export.println("}")
  }

  def addNode(name: String, attributes: List[(String, String)], level: Int) = {
    val node = new GraphNode(name, attributes)
    nodes = node :: nodes
    ranks.get(level) match {
      case Some(lst) => ranks.put(level, node :: lst)
      case None => ranks.put(level, List(node))
    }

    node.setId(counter)
    counter += 1

    node
  }

  def addNode() = {
    new GraphNode("", Nil)
  }

  /*
   * Create a link from @from to @to with label @label
   */

  def addLink(from: GraphNode, to: GraphNode, label: String) = {
    val link = new GraphLink(from, to, label)
    links = link :: links
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
