import scala.language.higherKinds
import scala.annotation._

package object opt {

  import compat._

  type Opt[+A] = Opt.Type[A]

  trait NotOpt[+A]

  implicit def neq[A]: NotOpt[A] = null.asInstanceOf[NotOpt[A]]
  @implicitAmbiguous("You can't store Opt inside of an Opt!")
  implicit def neqAmbig1[A]: NotOpt[Opt[A]] = ???
  implicit def neqAmbig2[A]: NotOpt[Opt[A]] = ???

  case class OuterOpt[+A](a: A)

  object Opt {

    type Base
    trait Tag extends Any
    type Type[+A] <: Base with Tag

    private[opt] class UnapplyOptOps[A](val optA: Opt[A]) extends AnyVal {
      @inline def isEmpty: Boolean = optA.isEmpty
      // this is guaranteed to be safe by compiler, pattern matches always call isEmpty first
      @inline def get: A = Opt unwrap optA
    }

//    @inline def apply[A, B](value: A)(implicit ev: A <:< Opt[B]): Type[A] = OuterOpt(value).asInstanceOf[Type[A]]
    @inline def apply[A: NotOpt](value: A): Type[A]        = value.asInstanceOf[Type[A]]
    @inline def unapply[A](optA: Opt[A]): UnapplyOptOps[A] = new UnapplyOptOps[A](optA)
    @inline def empty[A]: Type[A]                          = None.asInstanceOf[Type[A]]

    // unsafe
    private[opt] def unwrap[A](value: Type[A]): A = value.asInstanceOf[A]
  }

  object compat {
    import opt.Opt.UnapplyOptOps

    val None: Opt[Nothing] = null.asInstanceOf[Opt[Nothing]]
    object Some {
      @inline def apply[A: NotOpt](a: A): Opt[A]             = Opt(a)
      @inline def unapply[A](optA: Opt[A]): UnapplyOptOps[A] = new UnapplyOptOps[A](optA)
    }
  }

  implicit class OptSyntax[A](val optA: Opt[A]) extends AnyVal {
    @inline def isEmpty: Boolean   = optA == None
    @inline def isDefined: Boolean = optA != None

    @inline def contains(b: A): Boolean = optA match {
      case None    => false
      case Some(a) => a == b
    }

    @inline def exists(f: A => Boolean): Boolean = optA match {
      case None    => false
      case Some(a) => f(a)
    }

    @inline def map[B](f: A => B): Opt[B] = optA match {
      case None    => Opt.empty[B]
      case Some(a) => Opt(f(a))
    }

    @inline def flatMap[B](f: A => Opt[B]): Opt[B] = optA match {
      case None    => Opt.empty[B]
      case Some(a) => f(a)
    }

    @inline def filter(f: A => Boolean): Opt[A] = optA match {
      case None    => Opt.empty[A]
      case Some(a) => if (f(a)) Opt(a) else Opt.empty
    }

    @inline def filterNot(f: A => Boolean): Opt[A] = optA match {
      case None    => Opt.empty[A]
      case Some(a) => if (!f(a)) Opt(a) else Opt.empty
    }

    @inline def fold[B](z: B)(f: A => B): B = optA match {
      case None    => z
      case Some(a) => f(a)
    }

    @inline def flatten[B](implicit ev: A <:< Opt[B]): Opt[B] = optA match {
      case None    => Opt.empty
      case Some(a) => ev(a)
    }

    @inline def forall(f: A => Boolean): Boolean = optA.isEmpty || f(optA.get)

    @inline def foreach(f: A => Unit): Unit = optA match {
      case None    =>
      case Some(a) => f(a)
    }

    @inline def foldLeft[B](z: B)(f: (B, A) => B): B = {
      var res = z
      optA foreach (x => res = f(res, x))
      res
    }

    @inline def nonEmpty: Boolean = optA != None

    @inline def orElse(optB: Opt[A]): Opt[A] = if (optA.isEmpty) optB else optA

    @inline def orNull[A1 >: A](implicit ev: Null <:< A1): A1 = if (optA.isEmpty) ev(null) else optA.get

    @inline def foldRight[B](z: B)(f: (B, A) => B): B = foldLeft(z)(f)

    @inline def collect[B](pf: PartialFunction[A, B]): Opt[B] = optA match {
      case None    => Opt.empty
      case Some(a) => if (pf isDefinedAt a) Some(pf(a)) else Opt.empty
    }

    @inline def zip[B](optB: Opt[B]): Opt[(A, B)] = optA match {
      case None => None
      case Some(a) =>
        optB match {
          case Some(b) => Some((a, b))
          case None    => None
        }
    }

    @inline def unzip[A1, A2](implicit asPair: A => (A1, A2)): (Opt[A1], Opt[A2]) = optA match {
      case None =>
        (None, None)
      case Some(a) =>
        val t = asPair(a)
        (Some(t._1), Some(t._2))
    }

    @inline def unzip3[A1, A2, A3](implicit asPair: A => (A1, A2, A3)): (Opt[A1], Opt[A2], Opt[A3]) = optA match {
      case None =>
        (None, None, None)
      case Some(a) =>
        val t = asPair(a)
        (Some(t._1), Some(t._2), Some(t._3))
    }

    @inline def toList: List[A] = optA match {
      case None    => List.empty
      case Some(a) => List(a)
    }

    // allocates!
    @inline def toRight[L](left: => L): Either[L, A] = optA match {
      case None    => scala.util.Left(left)
      case Some(a) => scala.util.Right(a)
    }

    // allocates!
    @inline def toLeft[R](right: => R): Either[A, R] = optA match {
      case None    => scala.util.Right(right)
      case Some(a) => scala.util.Left(a)
    }

    // Accessors
    // Kill get before it gets out of hand?
    @inline def get: A = optA match {
      case None    => throw new NoSuchElementException("Opt.None.get")
      case Some(a) => a
    }

    @inline def getOrElse(f: => A): A = optA match {
      case None    => f
      case Some(a) => a
    }

    @inline def size: Int = optA match {
      case None    => 0
      case Some(_) => 1
    }
  }

}
