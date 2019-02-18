import opt._
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}

object testimplicits {

  import Gen.{const, frequency, resize, sized}

  /** Arbitrary instance of the Opt type */
  implicit def arbOpt[T](implicit a: Arbitrary[T]): Arbitrary[Opt[T]] =
    Arbitrary(sized(n =>
      // When n is larger, make it less likely that we generate None,
      // but still do it some of the time. When n is zero, we always
      // generate None, since it's the smallest value.
      frequency(
        (n, resize(n / 2, arbitrary[T]).map(Opt(_))),
        (1, const(Opt.empty)))))

}
