package tryscalaz7

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class EffectfulIteratorSpec extends FunSpec with ShouldMatchers {

  import scalaz._
  import Scalaz._

  describe("effectful iterator") {
    it("should collect") {

      // The first of these traversals accumulates elements effectfully, with an operation of type a → m (), but
      // modifies those elements purely and independently of this accumulation, with a function of type a → b.
      // collect :: (Traversable t, Applicative m) ⇒ (a → m ()) → (a → b) → t a → m (t b)
      // collect f g = traverse (λ a → pure (λ () → g a) ⊛ f a)

      def collect[T[_]:Traverse, A, B, S](f: A => B, t: T[A], g: S => S) =
        t.traverse[({type λ[x] = State[S,x]})#λ, B](a => State((s: S) => (g(s), f(a))))

      val loop = collect((a: Int) => 2 * a, List(10, 20, 30, 40), (i: Int) => i + 1)
      loop(0) should equal((4, List(20, 40, 60, 80)))   // (4,List(20, 40, 60, 80))
    } 

    it("should collect using mapAccumLeft") {
      def collect[A, B, S](f: A => B, g: S => S)(s: S, a: A) = (g(s), f(a))
      List(10, 20, 30, 40).mapAccumLeft(
        0, collect((a: Int) => 2 * a, (i: Int) => i + 1) _) should equal((4, List(20, 40, 60, 80)))
    }

    it("should collect using traverseS and runTraverseS combinators") {
      def collect[A, B, S](f: A => B, g: S => S) = (a: A) => State((s: S) => (g(s), f(a)))

      // traverseS
      List(10, 20, 30, 40).traverseS(
        collect((a: Int) => 2 * a, (i: Int) => i + 1)).run(0) should equal((4, List(20, 40, 60, 80)))

      // runTraverseS
      List(10, 20, 30, 40).runTraverseS(0)(collect((a: Int) => 2 * a, (i: Int) => i + 1)) should equal((4, List(20, 40, 60, 80)))
    }

    it("should disperse") {

      // disperse implementation corresponding to the same function in section 4.2 of the paper
      // Essence of the Iterator Pattern

      def disperse[T[_]:Traverse, A, S, B](t: T[A], s: A => State[S, B]) =
        t.traverse[({type λ[x] = State[S,x]})#λ, B](s)

      // implementing a labeling function, also from section 4.2
      // labeling every element with its position in order of traversal
      def label[T[_]:Traverse, A](t: T[A]) = disperse(t, ((a: A) => State((i: Int) => (i+1, i)))).eval(0)

      label(List(10, 20, 30, 40)) should equal(List(0, 1, 2, 3)) // List(0,1,2,3)

      // scalaz example of wordcount
      // look ma .. no type lambas
      def charLineCount[T[_]:Traverse](t: T[Char]) =
        disperse(t, ((a: Char) => State((counts: (Int, Int)) =>
          ((counts._1 + 1, counts._2 + (if (a == '\n') 1 else 0)), (counts._1, counts._2))))) eval (1,1)

      charLineCount("the cat in the hat\n sat on the mat\n".toList).last should equal((35, 2)) // (35,2)
    }
  }
}
