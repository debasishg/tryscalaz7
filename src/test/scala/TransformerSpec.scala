package tryscalaz7


import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TransformerSpec extends Spec with ShouldMatchers {

  import scalaz._

  describe("monad transformer 1") {

    it("using eitherT") {

      // implementations of typeclasses a dn the functions are in std
      // can do selective import

      import Either._
      import \/._
      import EitherT._
      import std.list._

      val e = eitherT[List, String, Int](List(1, 2, 3, 4, 5).map(\/.right(_)))
      e.toList should equal(List(List(1), List(2), List(3), List(4), List(5)))
      e.getOrElse(List.empty[Int]) should equal(List(1, 2, 3, 4, 5)) // not sure why it's List[Any]
      e.foldRight(0)(_ + _) should equal(15)
      e.map(_ * 2).getOrElse(List.empty[Int]) should equal(List(2, 4, 6, 8, 10))
      e.isRight.forall(_ == true) should equal(true)

      val bme = e.bimap(s => s ++ s, _ * 2)
      bme.toList should equal(List(List(2), List(4), List(6), List(8), List(10)))
      
      val f = eitherT[List, String, Int](List("debasish", "maulindu", "goutam", "kausik", "arun").map(\/.left(_)))
      val bmf = f.bimap(s => s ++ s, _ * 2)
      bmf.swap.toList should equal(List(List("debasishdebasish"), List("maulindumaulindu"), List("goutamgoutam"), List("kausikkausik"), List("arunarun")))
    }
  }
}
