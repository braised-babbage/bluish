package bluish
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import org.scalajs.dom
import dom.document
import org.scalajs.dom.html
import scala.util.Random



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

  def update(s: State, dt: Double): State = {
    val actors = s.actors.map(a => a.update(dt, s))
    var newState = State(s.level, actors, s.status)

    if (newState.status != Playing)
      return newState

    val player = newState.player

    if (s.level.touches(player.pos, player.size, Lava))
      return State(s.level, actors, Lost)

    for (actor <- actors) {
      if (actor != player && actor.overlaps(player))
        newState = actor.collide(newState)
    }
    newState
  }

  val dt = 20.0

  @JSExport
  def main(): Unit = {

    val (level,actors) = Level.parse("""
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

    var state = State(level, actors, Playing)
    var actorLayer = drawActors(state.actors)
    display.appendChild(actorLayer)

    def run() = {
      display.removeChild(actorLayer)
      state = update(state, dt)
      actorLayer = drawActors(state.actors)
      display.appendChild(actorLayer)
    }

    dom.window.setInterval(() => run, dt)
  }
}
