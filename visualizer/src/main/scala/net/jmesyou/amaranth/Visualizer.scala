package net.jmesyou.amaranth

import scala.quoted.*
import scala.tasty.inspector.*

object Visualizer extends Inspector {
  def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
    import quotes.reflect._

    val traversers = new Traversers(quotes)
    for (tasty <- tastys) {
      val tree = tasty.ast
      traversers.ClassTraverser.traverseTree(tree)(tree.symbol)
    }
  }


}
