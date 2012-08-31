package tryscalaz7


import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith


import scalaz.State
import scalaz.syntax.std.option._

// This is directly from Michael Pilquist's excellent presentation on Scalaz State Monad 
// https://speakerdeck.com/u/mpilquist/p/scalaz-state-monad

object social {
  trait SocialService {
    // type alias for convenience
    type StateCache[+A] = State[Cache, A]

    def followerStats(u: String): StateCache[FollowerStats]
  }

  case class FollowerStats(userName: String, 
                           numFollowers: Int, 
                           numFollowing: Int)

  case class Timestamped[A](value: A, timestamp: Long)

  case class Cache(stats: Map[String, Timestamped[FollowerStats]],
                   hits: Int,
                   misses: Int) {

    def get(userName: String) = stats get userName

    def update(u: String, s: Timestamped[FollowerStats]): Cache =
      Cache(stats + (u -> s), hits, misses)

    def recordHit: Cache = copy(hits = this.hits + 1)
    def recordMiss: Cache = copy(misses = this.misses + 1)
  }

  object FakeSocialService extends SocialService {

    def followerStats(u: String) = for {
      ofs <- checkCache(u)
      fs <- ofs.cata(State.state[Cache, FollowerStats], retrieve(u))
    } yield fs

    def checkCache(u: String): StateCache[Option[FollowerStats]] = for {

      // gets take the state and gives a value
      ofs <- State.gets {c: Cache => c.get(u).collect {case Timestamped(fs, ts) if !stale(ts) => fs}}

      // modify the state with a hit or a miss
      _ <- State.modify {c: Cache => ofs ? c.recordHit | c.recordMiss }
    } yield ofs

    private def stale(ts: Long): Boolean =
      System.currentTimeMillis - ts > (5 * 60 * 1000L)

    private def callWebService(u: String): FollowerStats =
      FollowerStats(u, 0, 0)

    private def retrieve(u: String): StateCache[FollowerStats] = for {
      fs <- State.state(callWebService(u))
      tfs = Timestamped(fs, System.currentTimeMillis)

      // write into the cache : update the state
      _ <- State.modify[Cache] { _.update(u, tfs) }
    } yield fs
  }
}

@RunWith(classOf[JUnitRunner])
class StateSpec extends FunSpec with ShouldMatchers {

  import social._
  import social.FakeSocialService._

  describe("state monad combinators") {

    it("threading state through applicatives") {
      import scalaz.syntax.traverse._
      import scalaz.std.list._

      val initCache = Cache(Map.empty[String, Timestamped[FollowerStats]], 0, 0)

      val listOfState: List[StateCache[FollowerStats]] =
        List(followerStats("debasish"), // miss
             followerStats("maulindu"), // miss
             followerStats("indrajit"), // miss
             followerStats("debasish"), // hit!
             followerStats("maulindu")) // hit!

      val stateOfList: StateCache[List[FollowerStats]] = listOfState.sequence[StateCache, FollowerStats]
      val (s, a) = stateOfList.run(initCache)
      s.hits should equal(2)
      s.misses should equal(3)
    }
  }
}
