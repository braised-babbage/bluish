package bluish

case class Vec(x: Double, y: Double){
  def +(p: Vec) = Vec(x + p.x, y + p.y)
  def *(d: Double) = Vec(x * d, y * d)
}

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


case class Level(background: Array[Array[Block]]) {

  def unparse: String = {
    val lines =
      for (row <- background)
      yield row.map(Block.char _).mkString("")

    lines.mkString("\n")
  }

  def width: Int = background(0).length
  def height: Int = background.length

  def blockAt(x: Int, y: Int) =
    background(y)(x)


  def touches(pos: Vec, size: Vec, blockType: Block) = {
    import math.{floor, ceil}
    val start = Vec(math.floor(pos.x), math.floor(pos.y))
    val end = Vec(math.ceil(pos.x + size.x), math.ceil(pos.y + size.y))
    val z = floor(pos.y)

    def r(s: Double, e: Double) = floor(s).toInt until ceil(e).toInt

    val blocks = for (
      y <- r(pos.y, pos.y + size.y);
      x <- r(pos.x, pos.x + size.x)
    ) yield {
      val outside = x < 0 || x >= width || y < 0 || y >= height
      if (outside) Wall else blockAt(x,y)
    }

    blocks.exists(_ == blockType)
  }
}


object Level {
  def parse(plan: String): (Level, Seq[Actor]) = {
    val rows = plan.trim.split("\n").map(_.trim)
    val width = rows(0).length
    val height = rows.length

    require(rows.forall(r => r.length == width), "level must be rectangular")

    val blocks =
      for (row <- rows)
      yield row.map(Block(_)).toArray


    val actors =
      for (
        (row, j) <- rows.zipWithIndex;
        (char, i) <- row.zipWithIndex
        if "@o|=v" contains char
      ) yield Actor(char)(i,j)

    (Level(blocks),actors)
  }
}
