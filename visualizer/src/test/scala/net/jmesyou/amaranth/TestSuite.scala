package net.jmesyou.amaranth

import munit._

class TestSuite extends FunSuite {
  test("hello world") {
    Main.main(Array("amaranth", "samples/target/scala-3.1.2/classes/samples/HelloWorld.tasty"))
  }
}
