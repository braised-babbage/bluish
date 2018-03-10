package bluish
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import org.scalajs.dom
import dom.document
import org.scalajs.dom.html
import scala.util.Random

case class KeyState(arrowLeft: Boolean, arrowRight: Boolean, arrowUp: Boolean)

@JSExportTopLevel("ScalaJSExample")
object ScalaJSExample {


  def update(s: State, dt: Double): State = {
    val actors = s.actors.map(a => a.update(dt, s, keys))
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

  var keys = KeyState(false, false, false)

  def track(e: dom.KeyboardEvent) = {
    if (e.key == "ArrowLeft") {
      keys = KeyState(e.`type` == "keydown", keys.arrowRight, keys.arrowUp)
      e.preventDefault()
    }
    if (e.key == "ArrowRight") {
      keys = KeyState(keys.arrowLeft, e.`type` == "keydown", keys.arrowUp)
      e.preventDefault()
    }
    if (e.key == "ArrowUp") {
      keys = KeyState(keys.arrowLeft, keys.arrowRight, e.`type` == "keydown")
      e.preventDefault()
    }
  }


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


    val display = Graphics.createElement(
      "div",
      Map("class" -> "game"),
      List(Graphics.drawGrid(level)))

    document.body.appendChild(display)

    var state = State(level, actors, Playing)
    var actorLayer = Graphics.drawActors(state.actors)
    display.appendChild(actorLayer)

    val dt = 20.0 // milliseconds

    def run() = {
      display.removeChild(actorLayer)
      state = update(state, dt/1000)
      actorLayer = Graphics.drawActors(state.actors)
      display.appendChild(actorLayer)
    }

    dom.window.addEventListener("keydown", track _)
    dom.window.addEventListener("keyup", track _)

    dom.window.setInterval(() => run, dt)
  }
}
