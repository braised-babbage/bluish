package bluish
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import org.scalajs.dom
import dom.document
import org.scalajs.dom.html
import scala.util.Random



@JSExportTopLevel("ScalaJSExample")
object ScalaJSExample {


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

    dom.window.setInterval(() => run, dt)
  }
}
