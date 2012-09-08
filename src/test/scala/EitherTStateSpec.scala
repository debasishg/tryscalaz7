package tryscalaz7


import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith


import scalaz._
import Scalaz._

// This is directly from Michael Pilquist's excellent presentation on Scalaz State Monad 
// https://speakerdeck.com/u/mpilquist/p/scalaz-state-monad

// explains an example where we mix EitherT and State monads
object Financial {
  case class Order(no: String, instruments: List[String])

  // order cache : The State in our example
  case class OrderCache(orders: Map[String, Order]) {
    def init = Map.empty[String, Order]
    def get(no: String) = orders get no
    def update(order: Order): OrderCache = OrderCache(orders + (order.no -> order))
  }

  type OrderCacheS[+A] = State[OrderCache, A]
  type ET[F[+_], A] = EitherT[F, String, A]
  type OrderCacheES[A] = ET[OrderCacheS, A]

  object OrderCacheES {
    def apply[A](st: OrderCacheS[String \/ A]): OrderCacheES[A] = EitherT(st)
    def liftE[A](ea: String \/ A): OrderCacheES[A] = apply(Pointed[OrderCacheS].point(ea))
    def liftS[A](st: OrderCacheS[A]): OrderCacheES[A] = MonadTrans[ET].liftM(st)
  }

  // check if the Order exists in the cache : note : stateful
  private def checkCache(no: String): OrderCacheS[Option[Order]] = {
    State.gets {c: OrderCache => c.get(no)}
  }

  // if not exists, need to get from an external source : 
  // note : stateful since we need to write to the cache if not found
  private def getExternal(no: String): OrderCacheS[Order] = for {
    os <- State.state(callWebService(no))

    // write into the cache : update the state
    _ <- State.modify[OrderCache] { _.update(os) }
  } yield os
  
  // dummy!
  private def callWebService(no: String) = Order(no, List("ibm", "google"))
    
  // client api :
  // 1. validate order no
  // 2. check if cache hit
  // 3. or else write to cache
  // and the whole thing is stateful
  def getOrder(no: String): OrderCacheES[Order] = {
    if (no.size == 0) OrderCacheES.liftE("Invalid order no".left)
    else {
      val s: OrderCacheS[Order] =
        for {
          maybeOrder <- checkCache(no)
          o <- maybeOrder.cata(State.state[OrderCache, Order], getExternal(no))
        } yield o
      EitherT.right[OrderCacheS, String, Order](s)
    }
  }
}

@RunWith(classOf[JUnitRunner])
class EitherTStateSpec extends FunSpec with ShouldMatchers {

  import Financial._
  import scalaz.syntax.traverse._
  import scalaz.std.list._

  describe("state with eitherT") {
    it("should work in the happy case") {
      val listOfState: List[OrderCacheES[Order]] =
        List(getOrder("100"),  // miss
             getOrder("200"),  // miss
             getOrder("100"),  // hit!
             getOrder("300"))  // miss

      val stateOfList: OrderCacheES[List[Order]] =
        listOfState.sequence[OrderCacheES, Order]
      
      val (s, v) = stateOfList run OrderCache(Map.empty[String, Order])
      s.orders.size should equal(3)
      v.toList.flatten.size should equal(4)
    }

    it("should work in failure case") {
      getOrder("").swap.toList.eval(OrderCache(Map.empty[String, Order])).head should equal("Invalid order no")
    }

    it("should work in failure case with multiple getOrders") {
      val listOfState: List[OrderCacheES[Order]] =
        List(getOrder("100"),  // miss
             getOrder("200"),  // miss
             getOrder(""),     // should fail
             getOrder("300"))  // miss

      val stateOfList: OrderCacheES[List[Order]] =
        listOfState.sequence[OrderCacheES, Order]
      
      val (s, v) = stateOfList run OrderCache(Map.empty[String, Order])
      s.orders.size should equal(3)
      v.swap.toList.head should equal("Invalid order no")
    }
  }
}

