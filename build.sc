import mill._, scalalib._

object core extends ScalaModule {
  def scalaVersion = "2.12.8"

  object test extends Tests {
    def ivyDeps        = Agg(
      ivy"org.scalacheck::scalacheck:1.14.0",
      ivy"org.scalatest::scalatest:3.0.5"
    )
    def testFrameworks = Seq(
      "org.scalacheck.ScalaCheckFramework",
      "org.scalatest.tools.Framework"
    )
  }
}
