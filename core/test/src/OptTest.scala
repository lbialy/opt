import org.scalacheck.Prop
import org.scalacheck.Properties
import org.scalacheck.Prop.AnyOperators

import opt._
import opt.compat._

object OptTest extends Properties("opt.Opt") {

  import testimplicits._

  property("map") = {
    Prop.forAll { (opt: Opt[Int], i: Int) =>
      val f: Int => Int = _ => i
      opt.map(f(_)) ?= {
        opt match {
          case Some(x) => Opt(f(x))
          case None    => Opt.empty
        }
      }
    }
  }

  property("flatMap") = {
    Prop.forAll { (option: Opt[Int], i: Int) =>
      val f: Int => Opt[Int] = _ => Opt(i)
      option.flatMap(f(_)) ?= {
        option match {
          case Some(x) => f(x)
          case None    => Opt.empty
        }
      }
    }
  }

  property("flatten") = {
    Prop.forAll { optionOfOption: Opt[Opt[Int]] =>
      optionOfOption.flatten[Int] ?= {
        optionOfOption match {
          case Some(Opt(i)) => Opt(i)
          case _            => Opt.empty
        }
      }
    }
  }

  property("foreach") = {
    Prop.forAll { (option: Opt[Int], unit: Unit) =>
      val proc: Int => Unit = _ => unit
      option.foreach(proc(_)) ?= {
        option match {
          case Some(x) => proc(x)
          case None    => ()
        }
      }
    }
  }

  property("fold") = {
    Prop.forAll { (option: Opt[Int], i: Int, y: Int) =>
      val f: Int => Int = _ => i
      option.fold(y)(f(_)) ?= {
        option match {
          case Some(x) => f(x)
          case None    => y
        }
      }
    }
  }

  property("foldLeft") = {
    Prop.forAll { (option: Opt[Int], i: Int, y: Int) =>
      val f: (Int, Int) => Int = (_, _) => i
      option.foldLeft(y)(f(_, _)) ?= {
        option match {
          case Some(x) => f(y, x)
          case None    => y
        }
      }
    }
  }

  property("foldRight") = {
    Prop.forAll { (option: Opt[Int], i: Int, y: Int) =>
      val f: (Int, Int) => Int = (_, _) => i
      option.foldRight(y)(f(_, _)) ?= {
        option match {
          case Some(x) => f(y, x)
          case None    => y
        }
      }
    }
  }

  property("collect") = {
    Prop.forAll { (option: Opt[Int], i: Int) =>
      val pf: PartialFunction[Int, Int] = {
        case x if x > 0 => i
      }
      option.collect(pf) ?= {
        option match {
          case Some(x) if pf.isDefinedAt(x) => Opt(pf(x))
          case _                            => Opt.empty
        }
      }
    }
  }

  property("isDefined") = {
    Prop.forAll { option: Opt[Int] =>
      option.isDefined ?= {
        option match {
          case Some(_) => true
          case None    => false
        }
      }
    }
  }

  property("isEmpty") = {
    Prop.forAll { option: Opt[Int] =>
      option.isEmpty ?= {
        option match {
          case Some(_) => false
          case None    => true
        }
      }
    }
  }

  property("nonEmpty") = {
    Prop.forAll { option: Opt[Int] =>
      option.nonEmpty ?= {
        option match {
          case Some(_) => true
          case None    => false
        }
      }
    }
  }

  property("orElse") = {
    Prop.forAll { (option: Opt[Int], y: Opt[Int]) =>
      option.orElse(y) ?= {
        option match {
          case Some(x) => Opt(x)
          case None    => y
        }
      }
    }
  }

  property("getOrElse") = {
    Prop.forAll { (option: Opt[Int], y: Int) =>
      option.getOrElse(y) ?= {
        option match {
          case Some(x) => x
          case None    => y
        }
      }
    }
  }

  property("get") = {
    Prop.forAll { option: Opt[Int] =>
      Prop.iff[Opt[Int]](option, {
        case Some(_) =>
          option.get ?= {
            option match {
              case Some(z) => z
              case None    => throw new Exception
            }
          }
        case None =>
          Prop.throws(classOf[Exception]) {
            option.get
          }
      })
    }
  }

  property("orNull") = {
    Prop.forAll { option: Opt[String] =>
      option.orNull ?= {
        option match {
          case Some(s) => s
          case None    => null
        }
      }
    }
  }

  property("filter") = {
    Prop.forAll { (option: Opt[Int], bool: Boolean) =>
      val pred: Int => Boolean = _ => bool
      option.filter(pred(_)) ?= {
        option match {
          case Some(x) if pred(x) => Some(x)
          case _                  => Opt.empty
        }
      }
    }
  }

  property("filterNot") = {
    Prop.forAll { (option: Opt[Int], bool: Boolean) =>
      val pred: Int => Boolean = _ => bool
      option.filterNot(pred(_)) ?= {
        option match {
          case Some(x) if !pred(x) => Some(x)
          case _                   => None
        }
      }
    }
  }

  property("exists") = {
    Prop.forAll { (option: Opt[Int], bool: Boolean) =>
      val pred: Int => Boolean = _ => bool
      option.exists(pred(_)) ?= {
        option match {
          case Some(x) => pred(x)
          case None    => false
        }
      }
    }
  }

  property("forall") = {
    Prop.forAll { (option: Opt[Int], bool: Boolean) =>
      val pred: Int => Boolean = _ => bool
      option.forall(pred(_)) ?= {
        option match {
          case Some(x) => pred(x)
          case None    => true
        }
      }
    }
  }

  property("contains") = {
    Prop.forAll { (option: Opt[Int], y: Int) =>
      option.contains(y) ?= {
        option match {
          case Some(x) => x == y
          case None    => false
        }
      }
    }
  }

  property("size") = {
    Prop.forAll { option: Opt[Int] =>
      option.size ?= {
        option match {
          case Some(_) => 1
          case None    => 0
        }
      }
    }
  }

  property("zip") = {
    Prop.forAll { (option1: Opt[Int], option2: Opt[Int]) =>
      option1.zip(option2) ?= {
        (option1, option2) match {
          case (Some(x), Some(y)) => Some((x, y))
          case _                  => None
        }
      }
    }
  }

  property("unzip") = {
    Prop.forAll { option: Opt[(Int, Int)] =>
      option.unzip ?= {
        option match {
          case Some((x, y)) => (Some(x), Some(y))
          case _            => (None, None)
        }
      }
    }
  }

  property("unzip3") = {
    Prop.forAll { option: Opt[(Int, Int, Int)] =>
      option.unzip3 ?= {
        option match {
          case Some((x, y, z)) => (Some(x), Some(y), Some(z))
          case _               => (None, None, None)
        }
      }
    }
  }

  property("toList") = {
    Prop.forAll { option: Opt[Int] =>
      option.toList ?= {
        option match {
          case Some(x) => List(x)
          case None    => Nil
        }
      }
    }
  }

  property("toRight") = {
    Prop.forAll { (option: Opt[Int], i: Int) =>
      option.toRight(i) ?= {
        option match {
          case Some(x) => scala.util.Right(x)
          case None    => scala.util.Left(i)
        }
      }
    }
  }

  property("toLeft") = {
    Prop.forAll { (option: Opt[Int], i: Int) =>
      option.toLeft(i) ?= {
        option match {
          case Some(x) => scala.util.Left(x)
          case None    => scala.util.Right(i)
        }
      }
    }
  }
}
