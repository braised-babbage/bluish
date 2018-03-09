package bluish
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import org.scalajs.dom
import dom.document
import org.scalajs.dom.html
import scala.util.Random

case class Point(x: Int, y: Int){
  def +(p: Point) = Point(x + p.x, y + p.y)
  def /(d: Int) = Point(x / d, y / d)
}

@JSExportTopLevel("ScalaJSExample")
object ScalaJSExample {

  def createElement(name: String, attr: Map[String, String], children: Seq[dom.Node] = Seq()): dom.Element = {
    val elt = document.createElement(name)

    for ((k,v) <- attr) elt.setAttribute(k, v)

    for (c <- children) elt.appendChild(c)

    elt
  }

  val scale = 20

  def cssClass(block: Block): String = block match {
    case Empty => "empty"
    case Wall => "wall"
    case Lava => "lava"
  }

  def cssClass(actor: Actor): String = "actor " + (actor match {
    case p: Player => "player"
    case c: Coin => "coin"
    case m: MovingLava => "lava"
  })

  def drawGrid(level: Level): dom.Element = {
    val blocks =
      for (row <- level.background)
      yield createElement(
        "tr",
        Map("height" -> s"${scale}px"),
        row.map(block => createElement("td", Map("class" -> cssClass(block))))
      )

    createElement("table",
      Map("class" -> "background", "style" -> s"width: ${level.width * scale}px"),
      blocks)
  }

  def style(actor: Actor, scale: Int): String =
    s"width: ${actor.width * scale}px; height: ${actor.height * scale}px; left: ${actor.x * scale}px; top: ${actor.y * scale}px;"

  def drawActors(level: Level): dom.Element = {
    val elts =
      for (actor <- level.actors) yield {
       createElement("div",
         Map(
           "class" -> cssClass(actor),
           "style" -> style(actor, scale)
         )
       )
      }
    createElement("div", Map(), elts)
  }

  @JSExport
  def main(): Unit = {

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


    val display = createElement(
      "div",
      Map("class" -> "game"),
      List(drawGrid(level)))

    document.body.appendChild(display)

    display.appendChild(drawActors(level))
    // dom.window.setInterval(() => run, 50)
  }
}
