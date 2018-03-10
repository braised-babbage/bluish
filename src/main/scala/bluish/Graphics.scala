package bluish

import org.scalajs.dom
import dom.document
import org.scalajs.dom.html

object Graphics {
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
    s"width: ${actor.size.x * scale}px; height: ${actor.size.y * scale}px; left: ${actor.pos.x * scale}px; top: ${actor.pos.y * scale}px;"

  def drawActors(actors: Seq[Actor]): dom.Element = {
    val elts =
      for (actor <- actors) yield {
       createElement("div",
         Map(
           "class" -> cssClass(actor),
           "style" -> style(actor, scale)
         )
       )
      }
    createElement("div", Map(), elts)
  }
}
