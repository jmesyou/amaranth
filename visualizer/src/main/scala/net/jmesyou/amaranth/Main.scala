package net.jmesyou.amaranth

import scala.tasty.inspector.TastyInspector

object Main {
  def main(args: Array[String]): Unit = {
    TastyInspector.inspectTastyFiles(args.tail.toList)(Visualizer)
  }

}
