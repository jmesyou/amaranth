package net.jmesyou.amaranth

import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef.*
import scalax.collection.GraphEdge.*
import scalax.collection.io.dot.*

import scala.collection.mutable
import scala.util.chaining.*
import implicits.*

import java.io.{BufferedWriter, File, FileWriter}

class Traversers[Q <: scala.quoted.Quotes & Singleton](val quotes: Q, val config: Main.Config) {

  import quotes.reflect._

  final val CTRL_FLOW_COLOR: String = "#ffb3b8"
  final val DATA_FLOW_COLOR: String = "#3ddbd9"
  final val READWRITE_COLOR: String = "#fa4d56"
  final val DEF_COLOR: String = "#9ef0f0"
  final val CONST_COLOR: String = "#d9fbfb"

  def escape(s: String): String = s.replace("<", "\\<").replace(">", "\\>")

  object Counter {
    var i = 0

    def getNext: Int = {
      val oldI = i
      i += 1
      oldI
    }

    def reset(): Unit = i = 0
  }

  case class GraphNode(index: Int, tree: Tree, owner: Option[GraphNode] = None) extends Ordering[GraphNode] {
    val id: Int = Counter.getNext

    def nodeId: NodeId = NodeId(nodeName + (id & 0xfffffff))

    override def compare(x: GraphNode, y: GraphNode): Int = x.index.compare(y.index)

    val nodeName: String = tree match {
      case _: Apply => "Apply"
      case _: Select => "Select"
      case _: Ident => "Ident"
      case _: New => "New"
      case _: Block => "Block"
      case _: Literal => "Literal"
      case _: ValDef => "ValDef"
      case _: While => "While"
      case _: Assign => "Assign"
      case _: Typed => "Typed"
      case _: TypeIdent => "TypeIdent"
      case _: Inferred => "Inferred"
      case _: Applied => "Applied"
    }

    def getLabel: Seq[DotAttr] = {
      val label = tree match {
        case ValDef(name, _, _) => "ValDef(" + name + ")"
        case Ident(string) => "Ident(" + string + ")"
        case Literal(const) => const match {
          case IntConstant(i: Int) => "Constant(" + i + ")"
          case StringConstant(s: String) => "Constant(" + "\"" + s + "\"" + ")"
          case ClassOfConstant(tpe: TypeRepr) => "Class(" + tpe.typeSymbol.name + ")"
        }
        case Select(_, name) => "Select(" + tree.symbol.owner.name + "." + escape(name) + ")"
        case _ => nodeName
      }
      Seq(DotAttr("label", id + " " + label))
    }

    def getStyle: Seq[DotAttr] = tree match {
      case While(_, _) | Block(_, _) =>
        Seq(
          DotAttr("style", "filled"),
          DotAttr("fillcolor",  CTRL_FLOW_COLOR)
        )
      case select: Select =>
        if (select.symbol.isDefDef)
          Seq(
            DotAttr("style", "filled"),
            DotAttr("fillcolor",  DEF_COLOR),
          )
        else
          Seq(
            DotAttr("style", "filled"),
            DotAttr("fillcolor",  READWRITE_COLOR),
            DotAttr("fontcolor", "white")
          )
      case Assign(_, _) | ValDef(_, _, _)  =>
        Seq(
          DotAttr("style", "filled"),
          DotAttr("fillcolor",  READWRITE_COLOR),
          DotAttr("fontcolor", "white")
        )
      case Apply(_, _) =>
        Seq(
          DotAttr("style", "filled"),
          DotAttr("fillcolor",  DATA_FLOW_COLOR),
          DotAttr("shape", "diamond")
        )
      case Literal(_) =>
        Seq(
          DotAttr("style", "filled"),
          DotAttr("fillcolor",  CONST_COLOR),
          DotAttr("shape", "ellipse")
        )
      case tree if tree.symbol.isType =>
        Seq(
          DotAttr("style", "rounded")
        )
      case _ => Nil
    }

    def nodeAttrs: Seq[DotAttr] = getLabel ++ getStyle

  }

  type TastyGraph = Graph[GraphNode, DiEdge]
  type GraphEdge = TastyGraph#EdgeT

  object ClassTraverser extends TreeTraverser {
    override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
      tree match {
        case ClassDef(_, _, _, _, body) =>
          body.foreach {
            case ddef: DefDef => graphDefDef(ddef)
          }
        case pkg@PackageClause(_, classes) =>
          this.foldTrees((), classes)(pkg.symbol)
        case _ => ()
      }
    }
  }

  def getRootGraph(id: Option[Id]): DotRootGraph = DotRootGraph(
    directed = false,
    id = id,
    attrStmts = List(
      DotAttrStmt(
        Elem.node,
        List(
          DotAttr("shape", "rect"),
          DotAttr("ordering", "in")
        )
      )
    )
  )

  def getSubGraph(root: DotRootGraph, nodeId: NodeId): DotSubGraph = DotSubGraph(
    ancestor = root,
    subgraphId = nodeId.id.toLowerCase,
    attrList = Seq(
      DotAttr("rank", "same")
    )
  )

  def edgeTransformer(root: DotRootGraph): EdgeTransformer[GraphNode, DiEdge] = innerEdge => innerEdge.edge match {
    case UnDiEdge(src, dst) =>
      val srcOuter = src.toOuter
      val dstOuter = dst.toOuter
      if (srcOuter.index == dstOuter.index - 1 && srcOuter.owner == dstOuter.owner)
        Some(
          root,
          DotEdgeStmt(src.toOuter.nodeId, dst.toOuter.nodeId, Seq(DotAttr("style", "invis")))
        )
      else if (srcOuter.tree.symbol.isTerm && dstOuter.tree.symbol.isType)
        Some(
          root,
          DotEdgeStmt(src.toOuter.nodeId, dst.toOuter.nodeId, Seq(DotAttr("style", "dashed")))
        )
      else
        Some(root, DotEdgeStmt(src.toOuter.nodeId, dst.toOuter.nodeId))
  }



  def graphDefDef(ddef: DefDef): Unit = {
    Counter.reset()

    val subGraphs: mutable.Map[Int, DotSubGraph] = mutable.Map.empty

    def getRankSubGraph(preds: List[GraphNode]): Option[DotSubGraph] = {
      preds foreach {
        pred => subGraphs.get(pred.id) match {
          case None => ()
          case some => return some
        }
      }
      None
    }

    def cNodeTransformer(root: DotRootGraph): NodeTransformer[GraphNode, DiEdge] =
      innerNode => {
        val outerNode = innerNode.toOuter
        val nodeId = outerNode.nodeId

        val preds = innerNode.diPredecessors.toList.map(_.toOuter)
        val ownerGraph = getRankSubGraph(preds) match {
          case None => root
          case Some(subgraph) => subgraph
        }

        Some(
          ownerGraph,
          DotNodeStmt(nodeId, innerNode.toOuter.nodeAttrs)
        )
      }

    ddef match {
      case DefDef(_, _, _, Some(body)) =>
        val fullName: String = ddef.symbol.fullName.replace('.', '_')
        val fileName = fullName + ".dot"

        given graph: TastyGraph = Graph.empty

        graphTree(None, body)

        graph.nodes.foreach {
          node =>
            val outerNode = node.toOuter
            val name = outerNode.nodeName
            if (name == "Block" || name == "Apply" || name == "Assign") {
              val subGraph = getSubGraph(root, outerNode.nodeId)
              subGraphs.put(outerNode.id, subGraph)
            }
        }

        given root: DotRootGraph = getRootGraph(Some(fullName))

        val dot = graph.toDot(root, edgeTransformer(root), cNodeTransformer = Some(cNodeTransformer(root)))

        dot.tap {
          text =>
            val file = new File(config.out.getPath + "/" + fileName)
            val bw = new BufferedWriter(new FileWriter(file))
            bw.write(text)
            bw.close()
        }
    }
  }

  def append(src: Option[GraphNode], dst: GraphNode)(using graph: TastyGraph): GraphNode = {
    src match {
      case None => assert(graph.add(dst))
      case Some(parent) => graph.add(parent ~> dst)
    }
    dst
  }

  /** Create edges between an ordered list of children nodes
   * This a useful helper for enforcing ordering between childrens of trees such a Blocks or Applies
   * These edges are later rendered visible by the edge transformer
   * @param owner The parent node
   * @param trees The children graph node
   * @param graph The graph
   * @return The last children node if the tree list is non empty.
   */
  def graphAndLinkTrees(owner: Option[GraphNode], trees: List[Tree])(using graph: TastyGraph): Option[GraphNode] = {
    trees
      .zipWithIndex
      .map {
        case (stat, idx) => graphTree(owner, stat, idx)
      }
      .pipe { children =>
        if (children.nonEmpty)
          Some(children.reduceLeft {
            case (src, dst) => append(Some(src), dst)
          })
        else
          None
      }
  }

  def graphTree(owner: Option[GraphNode], tree: Tree, index: Int = Int.MinValue)(using graph: TastyGraph): GraphNode = {
    val current = GraphNode(index, tree, owner)
    append(owner, current)
    tree match {
      case Block(stats: List[Tree@unchecked], expr) =>
        val blockEnd = graphAndLinkTrees(Some(current), stats)
        val last = graphTree(Some(current), expr, stats.length)

        blockEnd.foreach(end => append(Some(end), last))

      case Select(qualifier: Term, _) =>
        graphTree(Some(current), qualifier)

      case Apply(fun: Term, args: List[Term@unchecked]) =>
        graphAndLinkTrees(Some(current), fun :: args)

      case While(cond, body) =>
        graphAndLinkTrees(Some(current), cond :: List(body))

      case Assign(lhs, rhs) =>
        graphAndLinkTrees(Some(current), lhs :: List(rhs))

      case ValDef(_, tpt, rhs) =>
        graphTree(Some(current), tpt)
        rhs.foreach(term => graphTree(Some(current), term))

      case New(tpt) =>
        graphTree(Some(current), tpt)

      case Typed(term, tpt) =>
        graphTree(Some(current), term)
        graphTree(Some(current), tpt)

      case Applied(tpt, args) =>
        graphAndLinkTrees(Some(current), tpt :: args)

      case _: Ident | _: Literal | _: TypeIdent | _: Inferred => ()

      case tree => throw new UnsupportedOperationException(s"${tree.getClass}")
    }
    current
  }
}
