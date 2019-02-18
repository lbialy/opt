import org.scalatest.{FlatSpec, Matchers}

import opt._
import opt.compat._

class OptSpec extends FlatSpec with Matchers {

  "Opt" should "handle nesting equality correctly" in {
    val o1: Opt[Opt[Int]] = Opt(Opt(1))
    val o2: Opt[Opt[Int]] = Some(None)
    val o3: Opt[Opt[Int]] = None

    o1 shouldNot equal(o2)
    o2 shouldNot equal(o3)
    o1 shouldNot equal(o3)
  }

}
