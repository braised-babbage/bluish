package bluish.test

import utest._
import bluish._

object ActorTests extends TestSuite {
  val tests = Tests {
    "overlaps" - {
      // this is peeking @ Coin's size
      val a = new Coin(2,2)
      val b = new Coin(2.5,2.5)
      val c = new Coin(3,3)
      assert(a.overlaps(b))
      assert(b.overlaps(c))
      assert(!a.overlaps(c))
    }
  }
}
