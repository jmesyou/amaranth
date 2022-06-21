package net.jmesyou.amaranth

import java.io.File
import java.nio.file.Paths
import scala.tasty.inspector.TastyInspector

object Main {

  case class Config(
    out: File,
    files: List[File]
  )

  val parser = new scopt.OptionParser[Config]("amaranth") {
    head("amaranth", "0.1")

    help("help").text("prints this usage text")

    opt[File]('o', "out")
      .optional()
      .valueName("<file>")
      .action((x, c) => c.copy(out = x))
      .text("output directory")

    arg[File]("<file>.tasty ...")
      .unbounded()
      .required()
      .action((x, c) => c.copy(files = c.files :+ x))
      .text("required .tasty files")
  }

  def main(args: Array[String]): Unit = {
    val init = Config(Paths.get(".").toFile, List.empty)
    val config = parser.parse(args, init) match {
      case Some(config) => config
      case _ => sys.exit(-1)
    }
    TastyInspector.inspectTastyFiles(args.tail.toList)(new Visualizer(config))
  }

}
