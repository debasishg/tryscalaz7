package tryscalaz7


import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TransformerSpec extends FunSpec with ShouldMatchers {

  import scalaz._

  describe("monad transformer 1") {

    it("using eitherT") {

      // implementations of typeclasses and the functions are in std
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

  describe("monad transformer 2") {
    import Scalaz._
    import effect.IO
    import effect.IO._
    import Either._
    import EitherT._

    case class Person(lastName: String, firstName: String, age: Int)
    case class Address(no: Int, street: String, zip: String, city: String)

    // some validation functions
    def validAge(age: Int): Validation[String, Int] =
      if (age <= 0) "age must be > 0".failure
      else if (age > 100) "age must be <= 100".failure
      else age.success

    def validLastName(l: String): Validation[String, String] =
      if (l.isEmpty) "last name needs to be non-empty".failure
      else l.success

    // suppose we need to lookup the address in a hash map
    // lookup may fail : hence Option
    def getAddress(p: Person) =
      Address(12, "Monroe Street", "95050", "Cupertino").some // .point[IO]

    it("lack of composition without monad transformers") {

      import syntax.apply._

      // without monad transformer
      // uses an IO since in real life we will fetch it from database using the id
      def makePerson(id: Int) = 
        (validLastName("ghosh").toValidationNEL |@|
          validAge(25).toValidationNEL) {(l, a) => Person("debasish", l, a)}.disjunction.point[IO]

      // threatens to move off the right margin
      val a = makePerson(12) map {pd =>
        pd.map {p =>
          getAddress(p)
        }
      }
    }

    it("monad transformers make monads compose") {
      // using eitherT monad transfomer
      // uses an IO since in real life we will fetch it from database using the id
      def makePerson(id: Int) = 
        eitherT[IO, NonEmptyList[String], Person]((validLastName("ghosh").toValidationNEL |@|
          validAge(25).toValidationNEL) {(l, a) => Person("debasish", l, a)}.disjunction.point[IO])

      // much neater compared to the last usage
      val a = makePerson(12) map getAddress
      a.getOrElse(None).unsafePerformIO should equal(Some(Address(12,"Monroe Street","95050","Cupertino")))

      // failure case
      def makePersonF(id: Int) = 
        eitherT[IO, NonEmptyList[String], Person]((validLastName("").toValidationNEL |@|
          validAge(0).toValidationNEL) {(l, a) => Person("debasish", l, a)}.disjunction.point[IO])

      val ad = makePersonF(12) map getAddress
      ad.swap.getOrElse(NonEmptyList[String]("")).unsafePerformIO should equal(NonEmptyList("last name needs to be non-empty", "age must be > 0"))
    }

    it("moar transformers") {
      import scalaz.OptionT
      import OptionT._
      
      type EIO[+A] = EitherT[IO, NonEmptyList[String], A]

      def makePerson(id: Int) = 
        eitherT[IO, NonEmptyList[String], Person]((validLastName("ghosh").toValidationNEL |@|
          validAge(25).toValidationNEL) {(l, a) => Person("debasish", l, a)}.disjunction.point[IO])

      // using type alias
      val a: EIO[Option[Address]] = makePerson(12) map getAddress

      // wrap in a transformer
      val ad: OptionT[EIO, Address] = optionT[EIO](a)

      // we can now write combinators like the following
      ad map (_.city)
    }
  }

  describe("monad transformer 3") {
    it("should have stacked effects") {
      import scalaz._
      import Scalaz._

      // helpful aliases
      type EitherTString[M[+_], +A] = EitherT[M, String, A]
      type StateInt[+A] = State[Int, A]

      // state monad
      val incr: StateInt[Int] = State {s => (s + 1, s)}
      incr.replicateM(10).evalZero should equal(List(0,1,2,3,4,5,6,7,8,9))

      // stack with Free monad
      import Free.Trampoline
      incr.lift[Trampoline].replicateM(100000).evalZero.run.size should equal(100000)

      import \/._
      // stack with EitherTString
      MonadTrans[EitherTString].liftM(incr).run(0) should equal((1, right(0)))

      // stack with a List
      MonadTrans[EitherTString].liftM(incr.replicateM(5)).run(0) should equal((5, right(List(0, 1, 2, 3, 4))))
    }
  }
}
