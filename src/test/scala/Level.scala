package bluish.test

import utest._
import bluish._

object LevelTests extends TestSuite {
  val tests = Tests {
    val simpleLevel = """
          ......................
          ..#................#..
          ..#..............=.#..
          ..#.........o.o....#..
          ..#.@......#####...#..
          ..#####............#..
          ......#++++++++++++#..
          ......##############..
          ......................
        """

    "parseTrivial" - {
      val (level,actors) = Level.parse("""
         ..
         ..
      """)

      assert(actors.length == 0)
      assert(level.unparse == "..\n..")
    }
    "parseBigger" - {
      val (level,actors) = Level.parse(simpleLevel)

      val (bg,bga) = Level.parse(simpleLevel)

      // player, coin, coin, lava
      assert(actors.length == 4)
      assert(level.unparse == bg.unparse)
    }

    // todo: tests for a lot of stuff...
    "blockAt" - {
      val (level, actors) = Level.parse(simpleLevel)
      assert(level.blockAt(0,0) == Empty)
      assert(level.blockAt(2,1) == Wall)
    }

    "touches" - {
      val (level, actors) = Level.parse(simpleLevel)
      assert(level.touches(Vec(0,0), Vec(1,1), Empty))
      assert(level.touches(Vec(2,1), Vec(1,1), Wall))
      assert(level.touches(Vec(2,0), Vec(2,2), Empty))
      assert(level.touches(Vec(2,0), Vec(2,2), Wall))
    }
  }
}

