package bluish


sealed trait Block

object Empty extends Block
object Wall extends Block
object Lava extends Block

object Block {
  def apply(char: Char): Block =
    char match {
      case '.' => Empty
      case '#' => Wall
      case '+' => Lava
      case _   => Empty
    }

  def char(block: Block): Char =
    block match {
      case Empty => '.'
      case Wall => '#'
      case Lava => '+'
    }
}

sealed trait Actor {
  def x: Double
  def y: Double
  def width: Double 
  def height: Double
}

case class Player(x: Double, y: Double) extends Actor {
  val width = 0.8
  val height = 1.5
}
case class Coin(x: Double, y: Double) extends Actor {
  val width = 0.6
  val height = 0.6
}

sealed trait Path
object BounceVertical extends Path
object BounceHorizontal extends Path
object Drip extends Path
case class MovingLava(x: Double, y: Double, path: Path) extends Actor {
  val width = 1.0
  val height = 1.0
}


object Actor {
  def apply(char: Char)(i: Double, j: Double): Actor =
    char match {
      case '@' => Player(i,j)
      case 'o' => Coin(i,j)
      case '|' => MovingLava(i,j,BounceVertical)
      case '=' => MovingLava(i,j,BounceHorizontal)
      case 'v' => MovingLava(i,j,Drip)
    }
}


case class Level(
  background: Array[Array[Block]],
  actors: Seq[Actor] ) {

  def unparse: String = {
    val lines =
      for (row <- background)
      yield row.map(Block.char _).mkString("")

    lines.mkString("\n")
  }

  def width: Int = background(0).length
  def height: Int = background.length
}

object Level {
  def parse(plan: String): Level = {
    val rows = plan.trim.split("\n").map(_.trim)
    val width = rows(0).length
    val height = rows.length

    require(rows.forall(r => r.length == width), "level must be rectangular")

    val blocks =
      for (row <- rows)
      yield row.map(Block(_)).toArray

    def actor(char: Char)(i: Double, j: Double) =
    char match {
      case '@' => Player(i,j)
      case 'o' => Coin(i,j)
      case '|' => MovingLava(i,j,BounceVertical)
      case '=' => MovingLava(i,j,BounceHorizontal)
      case 'v' => MovingLava(i,j,Drip)
    }

    val actors =
      for (
        (row, j) <- rows.zipWithIndex;
        (char, i) <- row.zipWithIndex
        if "@o|=v" contains char
      ) yield actor(char)(i,j)

    Level(blocks, actors)
  }
}
