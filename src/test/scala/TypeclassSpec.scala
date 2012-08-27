package tryscalaz7


import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TypeclassSpec extends FunSpec with ShouldMatchers {

  import scalaz._

  describe("typeclass 1") {

    it("using typeclass impls under std") {

      // implementations of typeclasses a dn the functions are in std
      // can do selective import

      import std.option._, std.list._

      Apply[Option].map2(some(1), some(2))((a, b) => a + b) should equal(Some(3))
      Apply[List].ap(List(1,2,3,4))(List(((i: Int) => i * 2), ((i: Int) => i * 3))) should equal(List(2, 4, 6, 8, 3, 6, 9, 12))

      Traverse[List].sequence(List(some(1), some(2), some(3))) should equal(Some(List(1, 2, 3)))
      Traverse[List].sequence(List(some(1), some(2), none)) should equal(None)
      Traverse[List].traverse(List(1, 2, 3))(i => some(i)) should equal(Some(List(1, 2, 3)))
    }

    it("using syntax layer gives better type inference") {

      // using the syntax layer gives better type inference

      import std.list._
      import syntax.bind._

      List(List(1)).join should equal(List(1))
      List(List(1), List(2)).join should equal(List(1, 2))
      List(List(List(1), List(2))).join should equal(List(List(1), List(2)))
      List(List(List(1), List(2))).join.join should equal(List(1, 2))

      // import syntax.apply._

      // same example as above that uses ap directly
      List(1, 2, 3, 4) <*> List(((i: Int) => i * 2), (i: Int) => i * 3) should equal(List(2, 4, 6, 8, 3, 6, 9, 12))

      // same example as above that uses Apply[Option].map2
      import std.option._
      import syntax.applicative._
      some(1).map2(some(2))(_ + _) should equal(some(3))

      // import syntax.monad._
      val o1 = some(0)
      (o1 >>= (x => if (x == 0) some(0) else none)) should equal(some(0))

      import syntax.traverse._
      List(some(1), some(2), some(3)).sequence should equal(Some(List(1, 2, 3)))
    }

    it("playing around with Liskov") {
      import std.list._
      import syntax.bind._

      class Base(i: Int)
      case class Derived(j: Int, s: String) extends Base(j)

      // set up the Liskov instance
      implicitly[Derived <:< Base].apply(Derived(0, "")): Base

      List(List(Derived(0, "a"), Derived(1, "b"))).join[Base] should equal(List(Derived(0, "a"), Derived(1, "b")))
    }

    it("playing around with bind and join") {
      import std.option._
      import syntax.monad._

      // verifying alternative defn of monad using join
      // law : m >>= f = join(fmap f m)
      val f: Int => Option[Int] = (i => if (i == 0) some(0) else none)
      val o1 = some(0)
      (o1 >>= f) should equal((o1 map f).join)
    }
  }
}

