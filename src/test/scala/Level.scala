package bluish.test

import utest._
import bluish._

object LevelTests extends TestSuite {
  val tests = Tests {
    'parseTrivial - {
      val level = Level.parse("""
         ..
         ..
      """)

      assert(level.actors.length == 0)
      assert(level.unparse == "..\n..")
    }
    'parseBigger - {
        val level = Level.parse("""
          ......................
          ..#................#..
          ..#..............=.#..
          ..#.........o.o....#..
          ..#.@......#####...#..
          ..#####............#..
          ......#++++++++++++#..
          ......##############..
          ......................
        """)

      val bg = Level.parse("""
          ......................
          ..#................#..
          ..#................#..
          ..#................#..
          ..#........#####...#..
          ..#####............#..
          ......#++++++++++++#..
          ......##############..
          ......................
        """)

      // player, coin, coin, lava
      assert(level.actors.length == 4)
      assert(level.unparse == bg.unparse)
    }
  }
}
