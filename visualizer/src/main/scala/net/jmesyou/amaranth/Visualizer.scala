package net.jmesyou.amaranth

import net.jmesyou.amaranth.Main.Config

import scala.quoted.*
import scala.tasty.inspector.*

class Visualizer(config: Config) extends Inspector {
  def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit = {
    import quotes.reflect._

    val traversers = new Traversers(quotes, config)
    for (tasty <- tastys) {
      val tree = tasty.ast
      traversers.ClassTraverser.traverseTree(tree)(tree.symbol)
    }
  }


}
