package bluish

import org.scalajs.dom
import dom.document
import org.scalajs.dom.html


trait Graphics {
  def clear(): Unit
  def draw(state: State): Unit
  def update(state: State): Unit
  def scrollPlayerIntoView(state: State): Unit
}

case class KeyState(arrowLeft: Boolean, arrowRight: Boolean, arrowUp: Boolean)

trait KeyboardInterface {
  def keys: KeyState
}

trait UI extends Graphics with KeyboardInterface

class DomUI extends UI {
  import DomGraphics._
  val scale = 20
  val display = createElement("div", Map("class" -> "game"))
  var background: Option[dom.Element] = None
  var foreground: Option[dom.Element] = None


  document.body.appendChild(display)

  def clear(): Unit = {
    if (!background.isEmpty) {
      display.removeChild(background.get)
      background = None
    }
    if (!foreground.isEmpty) {
      display.removeChild(foreground.get)
      foreground = None
    }
  }

  def draw(s: State) = {
    background = Some(drawGrid(s.level, scale))
    for (bg <- background) display.appendChild(bg)
    update(s)
  }

  def update(state: State): Unit = {
    for (fg <- foreground) display.removeChild(fg)
    foreground = Some(drawActors(state.actors,scale))
    for (fg <- foreground) display.appendChild(fg)

    display.setAttribute("class", cssClass(state.status))
  }


  def scrollPlayerIntoView(state: State): Unit = {
    val width = display.clientWidth
    val height = display.clientHeight
    val margin = width / 3

    val left = display.scrollLeft
    val right = left + width
    val top = display.scrollTop
    val bottom = top + height

    val player = state.player
    val center = (player.pos + player.size * 0.5)*scale

    if (center.x < left + margin)
      display.scrollLeft = center.x - margin
    else if (center.x > right - margin)
      display.scrollLeft = center.x + margin - width


    if (center.y < top + margin)
      display.scrollTop = center.y - margin
    else if (center.y > bottom - margin)
      display.scrollTop = center.y + margin - height
  }


  var keys = KeyState(false, false, false)

  def track(e: dom.KeyboardEvent) = {
    e.key match {
      case "ArrowLeft" => {
        keys = KeyState(e.`type` == "keydown", keys.arrowRight, keys.arrowUp)
        e.preventDefault()}
      case "ArrowRight" => {
        keys = KeyState(keys.arrowLeft, e.`type` == "keydown", keys.arrowUp)
        e.preventDefault()}
      case "ArrowUp" => {
        keys = KeyState(keys.arrowLeft, keys.arrowRight, e.`type` == "keydown")
        e.preventDefault()}
      }
  }

  dom.window.addEventListener("keydown", track _)
  dom.window.addEventListener("keyup", track _)

}

object DomGraphics {
  def createElement(name: String, attr: Map[String, String], children: Seq[dom.Node] = Seq()): dom.Element = {
    val elt = document.createElement(name)

    for ((k,v) <- attr) elt.setAttribute(k, v)

    for (c <- children) elt.appendChild(c)

    elt
  }

  def cssClass(block: Block): String = block match {
    case Empty => "empty"
    case Wall => "wall"
    case Lava => "lava"
  }

  def cssClass(status: GameStatus): String = status match {
    case Playing => "game playing"
    case Won => "game won"
    case Lost => "game lost"
    case _ => "game"
  }

  def cssClass(actor: Actor): String = "actor " + (actor match {
    case p: Player => "player"
    case c: Coin => "coin"
    case m: MovingLava => "lava"
  })

  def drawGrid(level: Level, scale: Int): dom.Element = {
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

  def drawActors(actors: Seq[Actor], scale: Int): dom.Element = {
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
